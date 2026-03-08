mod resolver_error;
pub mod var_map;

use crate::{
    ast::{
        ASTVisitor, BlockItem, FunctionDef, Program, VarDeclaration,
        expr::{AssignExpr, Expr, ExprVisitor, UnaryOp},
        stmt::{Stmt, StmtVisitor},
    },
    resolver::{
        resolver_error::{ResolverError, ResolverErrorType, ResolverWarning, ResolverWarningType},
        var_map::VarMap,
    },
    source_file::SourceFile,
    symbol::{Symbol, SymbolTable},
    with_token::WithToken,
};
use anyhow;

#[derive(Debug)]
pub struct Resolver<'a> {
    current_function: Option<WithToken<String>>,
    current_function_return: bool,
    source_file: &'a SourceFile,
    symbol_table: SymbolTable,
    var_map: VarMap,
    warnings: Vec<ResolverWarning>,
}

type ResolverResult<T> = anyhow::Result<T, ResolverError>;

impl<'a> Resolver<'a> {
    pub fn new(symbol_table: SymbolTable, source_file: &'a SourceFile) -> Self {
        Resolver {
            current_function: None,
            current_function_return: false,
            var_map: VarMap::new(),
            source_file,
            symbol_table,
            warnings: Vec::new(),
        }
    }

    pub fn symbol_table(&self) -> &VarMap {
        &self.var_map
    }

    pub fn release_symbol_table(self) -> VarMap {
        self.var_map
    }

    pub fn warnings(&self) -> &[ResolverWarning] {
        &self.warnings
    }

    fn updated_variable_name<T>(&self, name: &str, token: &WithToken<T>) -> String {
        // need to assign a unique name to each variable declaration to avoid name clashes in SymbolTable
        let token = token.get_token(&self.source_file.get_tokens_checked());
        format!("lv.{}.{}_{}", name, token.begin.0, token.end.0)
    }

    fn resolve_st_assert_exists(&self, name: &Symbol) -> ResolverResult<&str> {
        let name_str = self
            .symbol_table
            .resolve(*name)
            .expect("identifier should have been interned during lexing");
        Ok(name_str)
    }
}

impl<'a> ExprVisitor<ResolverResult<Expr>> for Resolver<'a> {
    fn visit_assignment_expr(
        &mut self,
        expr: crate::ast::expr::AssignExpr,
    ) -> ResolverResult<Expr> {
        let AssignExpr {
            target: left,
            eq_token: tok,
            right,
        } = expr;

        if let Expr::Variable(name) = *left {
            let name_str = self.resolve_st_assert_exists(&name.item)?;

            if let Some(resolved_sym) = self.var_map.get(&name_str) {
                let right = Box::new(self.visit_expr(*right)?);
                Ok(Expr::Assignment(AssignExpr {
                    target: Box::new(Expr::Variable(name.with_value(*resolved_sym))),
                    right,
                    eq_token: tok,
                }))
            } else {
                let token = tok.get_token(&self.source_file.get_tokens_checked());
                Err(ResolverError {
                    err_type: ResolverErrorType::UndefinedVariable(name_str.to_string()),
                    span: (token.begin, token.end),
                })
            }
        } else {
            let token = tok.get_token(&self.source_file.get_tokens_checked());
            Err(ResolverError {
                err_type: ResolverErrorType::InvalidLValue,
                span: (token.begin, token.end),
            })
        }
    }

    fn visit_binary_expr(&mut self, expr: crate::ast::expr::BinaryExpr) -> ResolverResult<Expr> {
        let left = self.visit_expr(*expr.left)?;
        let right = self.visit_expr(*expr.right)?;
        Ok(Expr::Binary(crate::ast::expr::BinaryExpr {
            left: Box::new(left),
            operator: expr.operator,
            right: Box::new(right),
        }))
    }

    fn visit_constant(
        &mut self,
        expr: WithToken<crate::lexer::token::Literal>,
    ) -> ResolverResult<Expr> {
        Ok(Expr::Constant(expr))
    }

    fn visit_unary_expr(&mut self, expr: crate::ast::expr::UnaryExpr) -> ResolverResult<Expr> {
        if let UnaryOp::Decrement | UnaryOp::Increment = *expr.operator {
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

        Ok(Expr::Unary(crate::ast::expr::UnaryExpr {
            operator: expr.operator,
            operand: Box::new(operand),
            postfix: expr.postfix,
        }))
    }

    fn visit_variable(&mut self, var: WithToken<Symbol>) -> ResolverResult<Expr> {
        let name_str = self.resolve_st_assert_exists(&var.item)?;
        if let Some(resolved_sym) = self.var_map.get(name_str) {
            Ok(Expr::Variable(resolved_sym))
        } else {
            Err(ResolverError {
                err_type: ResolverErrorType::UndefinedVariable(name_str.to_string()),
                span: var.get_token(&self.source_file.get_tokens_checked()).span(),
            })
        }
    }
}

impl<'a> StmtVisitor<ResolverResult<Stmt>> for Resolver<'a> {
    fn visit_expr_stmt(&mut self, expr: Expr) -> ResolverResult<Stmt> {
        let expr = self.visit_expr(expr)?;
        Ok(Stmt::Expr(expr))
    }

    fn visit_null_stmt(&mut self) -> ResolverResult<Stmt> {
        Ok(Stmt::Null)
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
}

impl<'a> ASTVisitor for Resolver<'a> {
    type StmtResult = ResolverResult<Stmt>;
    type ExprResult = ResolverResult<Expr>;
    type BlockItemResult = ResolverResult<BlockItem>;
    type FunctionDefResult = ResolverResult<FunctionDef>;
    type ProgramResult = ResolverResult<Program>;
    type VarDeclResult = ResolverResult<VarDeclaration>;

    fn visit_program(&mut self, program: Program) -> Self::ProgramResult {
        let mut resolved_function_defs = Vec::new();
        for func_def in program.function_defs {
            resolved_function_defs.push(self.visit_function_def(func_def)?);
        }
        Ok(Program {
            function_defs: resolved_function_defs,
        })
    }

    fn visit_function_def(&mut self, func_def: FunctionDef) -> Self::FunctionDefResult {
        self.current_function = Some(func_def.name.clone());
        self.current_function_return = false;

        let mut resolved_body = None;
        if let Some(body) = func_def.body {
            let mut resolved_block_items = Vec::new();
            for item in body {
                resolved_block_items.push(self.visit_block_item(item)?);
            }
            resolved_body = Some(resolved_block_items);

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
        }

        Ok(FunctionDef {
            name: func_def.name,
            body: resolved_body,
        })
    }

    fn visit_block_item(&mut self, item: BlockItem) -> Self::BlockItemResult {
        match item {
            BlockItem::Stmt(stmt) => Ok(BlockItem::Stmt(self.visit_stmt(stmt)?)),
            BlockItem::Decl(decl) => Ok(BlockItem::Decl(self.visit_var_decl(decl)?)),
        }
    }

    fn visit_var_decl(&mut self, var_decl: VarDeclaration) -> Self::VarDeclResult {
        let name_str = self.resolve_st_assert_exists(&var_decl.name.item)?;
        let updated_name = self.updated_variable_name(name_str, &var_decl.name);

        if let Some(sym) = self.var_map.get(&name_str) {
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
