mod resolver_error;
pub mod var_map;

use std::collections::HashMap;

use crate::{
    ast::{
        self, FunctionDef, VarDeclaration,
        expr::{AssignExpr, Expr},
        folder::ASTFolder,
        stmt::{self, Stmt},
    },
    resolver::{
        resolver_error::{ResolverError, ResolverErrorType, ResolverWarning, ResolverWarningType},
        var_map::ScopedVarMap,
    },
    source_file::SourceFile,
    symbol::{Symbol, SymbolTable},
    with_token::WithToken,
};

pub type LabelMap = HashMap<String, WithToken<()>>;

#[derive(Debug)]
pub struct Resolver<'a> {
    current_function: Option<WithToken<String>>,
    current_function_return: bool,
    labels: LabelMap,
    source_file: &'a SourceFile,
    symbol_table: SymbolTable,
    var_map: ScopedVarMap,
    warnings: Vec<ResolverWarning>,
}

pub type ResolverResult<T> = anyhow::Result<T, ResolverError>;

impl<'a> Resolver<'a> {
    pub fn new(symbol_table: SymbolTable, source_file: &'a SourceFile) -> Self {
        Resolver {
            current_function: None,
            current_function_return: false,
            labels: LabelMap::new(),
            var_map: ScopedVarMap::new(),
            source_file,
            symbol_table,
            warnings: Vec::new(),
        }
    }

    pub fn symbol_table(&self) -> &ScopedVarMap {
        &self.var_map
    }

    pub fn release_symbol_table_and_labels(self) -> (ScopedVarMap, LabelMap) {
        (self.var_map, self.labels)
    }

    pub fn warnings(&self) -> &[ResolverWarning] {
        &self.warnings
    }

    fn updated_variable_name<T>(&self, name: &str, token: &WithToken<T>) -> String {
        // need to assign a unique name to each variable declaration to avoid name clashes in SymbolTable
        let token = token.get_token(&self.source_file.get_tokens_checked());
        format!("lv.{}.{}_{}", name, token.begin.0, token.end.0)
    }

    fn updated_label(&self, name: &str) -> String {
        format!("L.{}", name)
    }

    fn resolve_st_assert_exists(&self, name: &Symbol) -> ResolverResult<&str> {
        let name_str = self
            .symbol_table
            .resolve(*name)
            .expect("identifier should have been interned during lexing");
        Ok(name_str)
    }

    fn begin_scope(&mut self) {
        self.var_map.add_scope();
    }

    fn end_scope(&mut self) {
        self.var_map.unwind();
    }
}

impl<'a> ASTFolder<ResolverError> for Resolver<'a> {
    fn visit_assignment_expr(
        &mut self,
        expr: ast::expr::AssignExpr,
    ) -> Result<Expr, ResolverError> {
        let AssignExpr {
            target: left,
            eq_token,
            right,
        } = expr;

        if let Expr::Variable(_) = *left {
            let left = self.visit_expr(*left)?;
            let right = self.visit_expr(*right)?;
            Ok(Expr::Assignment(AssignExpr {
                target: Box::new(left),
                eq_token,
                right: Box::new(right),
            }))
        } else {
            let token = eq_token.get_token(&self.source_file.get_tokens_checked());
            Err(ResolverError {
                err_type: ResolverErrorType::InvalidLValue,
                span: (token.begin, token.end),
            })
        }
    }

    fn visit_unary_expr(&mut self, expr: ast::expr::UnaryExpr) -> Result<Expr, ResolverError> {
        if let ast::expr::UnaryOp::Decrement | ast::expr::UnaryOp::Increment = *expr.operator {
            if let Expr::Variable(_) = *expr.operand {
            } else {
                let token = expr
                    .operator
                    .get_token(&self.source_file.get_tokens_checked());
                return Err(ResolverError {
                    err_type: ResolverErrorType::InvalidLValue,
                    span: (token.begin, token.end),
                });
            }
        }
        let operand = self.visit_expr(*expr.operand)?;

        Ok(Expr::Unary(ast::expr::UnaryExpr {
            operator: expr.operator,
            operand: Box::new(operand),
            postfix: expr.postfix,
        }))
    }

    fn visit_variable(&mut self, var: WithToken<Symbol>) -> ResolverResult<Expr> {
        let name_str = self.resolve_st_assert_exists(&var.item)?;
        if let Some(resolved_sym) = self.var_map.lookup(&name_str) {
            Ok(Expr::Variable(resolved_sym))
        } else {
            Err(ResolverError {
                err_type: ResolverErrorType::UndefinedVariable(name_str.to_string()),
                span: var.get_token(&self.source_file.get_tokens_checked()).span(),
            })
        }
    }

    fn visit_for_stmt(&mut self, stmt: ast::stmt::ForStmt) -> Result<Stmt, ResolverError> {
        self.begin_scope();

        let initializer = stmt
            .initializer
            .map_option_transpose_result(|init| match init {
                stmt::ForStmtInit::Declaration(decls) => decls
                    .into_iter()
                    .map(|decl| self.visit_var_decl(decl))
                    .collect::<Result<Vec<_>, _>>()
                    .map(stmt::ForStmtInit::Declaration),
                stmt::ForStmtInit::Expression(expr) => {
                    Ok(stmt::ForStmtInit::Expression(self.visit_expr(expr)?))
                }
            })?;

        let condition = stmt
            .condition
            .map_option_transpose_result(|cond| self.visit_expr(cond))?;

        let step = stmt
            .step
            .map_option_transpose_result(|cond| self.visit_expr(cond))?;

        let body = Box::new(self.visit_stmt(*stmt.body)?);
        self.end_scope();

        Ok(Stmt::For(stmt::ForStmt {
            loop_id: stmt.loop_id,
            initializer,
            condition,
            step,
            body,
        }))
    }

    fn visit_goto_stmt(&mut self, stmt: WithToken<String>) -> ResolverResult<Stmt> {
        let updated_label_name = stmt.map(|label| self.updated_label(&label));
        Ok(Stmt::Goto(updated_label_name))
    }

    fn visit_label_stmt(&mut self, stmt: ast::stmt::Label) -> ResolverResult<Stmt> {
        let ast::stmt::Label { name, next_stmt } = stmt;
        let updated_name = self.updated_label(&name.item);

        if let Some(prev_sym) = self.labels.get(&updated_name) {
            let prev_token = prev_sym.get_token(self.source_file.get_tokens_checked());
            let token = name.get_token(self.source_file.get_tokens_checked());
            let line_col = self.source_file.line_col(prev_token.begin.0);
            Err(ResolverError {
                err_type: ResolverErrorType::DuplicateLabel {
                    label: name.item,
                    prev_line: line_col.0,
                    prev_col: line_col.1,
                },
                span: token.span(),
            })
        } else {
            self.labels
                .insert(updated_name.clone(), name.with_value(()));
            let new_next_stmt = self.visit_stmt(*next_stmt)?;
            Ok(Stmt::Label(ast::stmt::Label {
                name: name.with_value(updated_name),
                next_stmt: Box::new(new_next_stmt),
            }))
        }
    }

    fn visit_return_stmt(&mut self, stmt: WithToken<Option<Expr>>) -> ResolverResult<Stmt> {
        if let Some(_) = &self.current_function {
            self.current_function_return = true;
        } else {
            let token = stmt.get_token(&self.source_file.get_tokens_checked());
            return Err(ResolverError {
                err_type: ResolverErrorType::ReturnOutsideFunction,
                span: token.span(),
            });
        };

        let with_token = stmt.with_value(());
        let resolved_expr = if let Some(expr) = stmt.item {
            Some(self.visit_expr(expr)?)
        } else {
            None
        };
        Ok(Stmt::Return(with_token.with_value(resolved_expr)))
    }

    fn visit_block(&mut self, block: ast::Block) -> Result<ast::Block, ResolverError> {
        self.begin_scope();
        let body = block
            .body
            .into_iter()
            .map(|item| self.visit_block_item(item))
            .collect::<Result<Vec<_>, _>>()?;
        self.end_scope();

        Ok(ast::Block {
            body,
            block_begin: block.block_begin,
        })
    }

    fn visit_function_def(&mut self, func_def: FunctionDef) -> ResolverResult<FunctionDef> {
        self.current_function = Some(func_def.name.clone());
        self.current_function_return = false;

        let resolved_body = if let Some(body) = func_def.body {
            let resolved_body = self.visit_block(body)?;

            if !self.current_function_return && func_def.name.item != "main" {
                let token = func_def
                    .name
                    .get_token(&self.source_file.get_tokens_checked());
                self.warnings.push(ResolverWarning {
                    warn_type: ResolverWarningType::NonVoidFunctionNoReturn(
                        func_def.name.item.clone(),
                    ),
                    location: token.span(),
                });
            }
            Some(resolved_body)
        } else {
            None
        };

        Ok(FunctionDef {
            name: func_def.name,
            body: resolved_body,
        })
    }

    fn visit_var_decl(&mut self, var_decl: VarDeclaration) -> ResolverResult<VarDeclaration> {
        let name_str = self.resolve_st_assert_exists(&var_decl.name.item)?;
        let updated_name = self.updated_variable_name(name_str, &var_decl.name);

        if let Some(sym) = self.var_map.current_scope().get(name_str) {
            let cur_decl_token = var_decl
                .name
                .get_token(&self.source_file.get_tokens_checked());

            let prev_line_col = self.source_file.line_col_token_begin(sym.token_id);
            return Err(ResolverError {
                err_type: ResolverErrorType::VariableAlreadyDefined {
                    name: name_str.to_string(),
                    prev_line: prev_line_col.0,
                    prev_col: prev_line_col.1,
                },
                span: (cur_decl_token.begin, cur_decl_token.end),
            });
        }

        let updated_sym = self.var_map.insert(
            name_str.to_string(),
            updated_name,
            var_decl.name.with_value(()),
        );

        let resolved_init = if let Some(init) = var_decl.initializer {
            let with_token = init.with_value(());
            Some(with_token.with_value(self.visit_expr(init.unwrap())?))
        } else {
            None
        };

        Ok(VarDeclaration {
            name: var_decl.name.with_value(updated_sym),
            initializer: resolved_init,
        })
    }
}
