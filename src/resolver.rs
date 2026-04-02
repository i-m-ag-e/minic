pub mod resolver_error;
pub mod var_map;

use std::collections::HashMap;

use crate::{
    ast::{self, FunctionDecl, VarDeclaration, expr::Expr, folder::ASTFolder, stmt::Stmt},
    resolver::{
        resolver_error::{ResolverError, ResolverErrorType, ResolverWarning, ResolverWarningType},
        var_map::{Linkage, MapEntry, ScopedVarMap},
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

    fn make_error<T>(&self, err_type: ResolverErrorType, token: WithToken<T>) -> ResolverError {
        let span = token
            .get_token(&self.source_file.get_tokens_checked())
            .span();
        ResolverError { err_type, span }
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
        if let Expr::Variable(_) = *expr.target {
            self.fold_assignment_expr(expr)
        } else {
            let token = expr
                .eq_token
                .get_token(&self.source_file.get_tokens_checked());
            Err(ResolverError {
                err_type: ResolverErrorType::InvalidLValue,
                span: (token.begin, token.end),
            })
        }
    }

    fn visit_function_call(
        &mut self,
        expr: ast::expr::FunctionCall,
    ) -> Result<Expr, ResolverError> {
        let callee_expr = expr
            .callee_expr
            .map_transpose_result(|e| self.visit_expr(*e))?
            .into_boxed();
        let args = expr
            .args
            .into_iter()
            .map(|arg| arg.map_transpose_result(|e| self.visit_expr(e)))
            .collect::<Result<_, _>>()?;

        Ok(Expr::FunctionCall(ast::expr::FunctionCall {
            callee_expr,
            args,
        }))
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
            Ok(Expr::Variable(var.with_value(resolved_sym.name.item)))
        } else {
            Err(ResolverError {
                err_type: ResolverErrorType::UndefinedVariable(name_str.to_string()),
                span: var.get_token(&self.source_file.get_tokens_checked()).span(),
            })
        }
    }

    fn visit_for_stmt(&mut self, stmt: ast::stmt::ForStmt) -> Result<Stmt, ResolverError> {
        self.begin_scope();
        let result = self.fold_for_stmt(stmt)?;
        self.end_scope();

        Ok(result)
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
        let introduce_scope = block.introduce_scope;

        if introduce_scope {
            self.begin_scope();
        }
        let result = self.fold_block(block)?;

        if introduce_scope {
            self.end_scope();
        }

        Ok(result)
    }

    fn visit_function_def(&mut self, func_def: FunctionDecl) -> ResolverResult<FunctionDecl> {
        let name_str = self
            .resolve_st_assert_exists(&func_def.name.item)?
            .to_string();

        self.current_function = Some(func_def.name.with_value(name_str.clone()));
        self.current_function_return = false;

        if let Some(prev_entry) = self.var_map.lookup_in_current_scope(&name_str)
            && prev_entry.linkage == Linkage::None
        {
            let (prev_line, prev_col) = self
                .source_file
                .line_col_token_begin(prev_entry.name.token_id);
            return Err(self.make_error(
                ResolverErrorType::RedeclaredAsDifferentKind {
                    name: name_str.to_string(),
                    new_kind: "function",
                    prev_kind: "local varable",
                    prev_line,
                    prev_col,
                },
                func_def.name,
            ));
        }

        let prev_entry = self.var_map.lookup(&name_str).unwrap_or(MapEntry {
            name: func_def.name.with_value(Symbol(0)),
            linkage: Linkage::None,
            defined: false,
        });

        let new_symbol = self.var_map.insert(
            name_str.clone(),
            name_str.clone(),
            func_def.name.with_value(()),
            Linkage::External,
            func_def.body.is_some(),
        );

        self.begin_scope();

        let params = func_def
            .params
            .into_iter()
            .map(|param| {
                let token = param.with_value(());
                if let Some(param_var) = param.item {
                    self.visit_var_decl(VarDeclaration {
                        name: token.with_value(param_var),
                        initializer: None,
                    })
                    .map(|var_decl| var_decl.name.map(Some))
                } else {
                    Ok(token.with_value(None))
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        let resolved_body = if let Some(body) = func_def.body {
            if prev_entry.defined {
                let (prev_line, prev_col) = self
                    .source_file
                    .line_col_token_begin(prev_entry.name.token_id);
                return Err(self.make_error(
                    ResolverErrorType::RedefinedFunction {
                        name: name_str.to_string(),
                        prev_line,
                        prev_col,
                    },
                    func_def.name,
                ));
            }

            let resolved_body = self.visit_block(body)?;

            if !self.current_function_return && name_str != "main" {
                let token = func_def
                    .name
                    .get_token(&self.source_file.get_tokens_checked());
                self.warnings.push(ResolverWarning {
                    warn_type: ResolverWarningType::NonVoidFunctionNoReturn(name_str.to_string()),
                    location: token.span(),
                });
            }
            Some(resolved_body)
        } else {
            None
        };

        self.end_scope();

        Ok(FunctionDecl {
            name: func_def.name.with_value(new_symbol),
            body: resolved_body,
            params,
        })
    }

    fn visit_var_decl(&mut self, var_decl: VarDeclaration) -> ResolverResult<VarDeclaration> {
        let name_str = self.resolve_st_assert_exists(&var_decl.name.item)?;
        let updated_name = self.updated_variable_name(name_str, &var_decl.name);

        if let Some(sym) = self.var_map.current_scope().get(name_str) {
            let cur_decl_token = var_decl
                .name
                .get_token(&self.source_file.get_tokens_checked());

            let prev_line_col = self.source_file.line_col_token_begin(sym.name.token_id);
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
            Linkage::None,
            true,
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
