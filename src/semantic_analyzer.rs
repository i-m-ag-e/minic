use crate::{
    ast::{
        self, ASTVisitor,
        expr::{self, ExprVisitor},
        stmt::{self, StmtVisitor},
    },
    source_file::SourceFile,
    with_token::WithToken,
};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Undefined label: {0}")]
    UndefinedLabel(String),
}

#[derive(Error, Debug)]
#[error("Semantic error: {err}")]
pub struct SemanticAnalyzerError {
    pub err: SemanticError,
    pub span: (usize, usize),
}

#[derive(Debug)]
pub struct SemanticAnalyzer<'a> {
    source_file: &'a SourceFile,
    labels: HashMap<String, WithToken<()>>,
}

pub type SemanticResult<T> = Result<T, SemanticAnalyzerError>;

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(source_file: &'a SourceFile, labels: HashMap<String, WithToken<()>>) -> Self {
        Self {
            source_file,
            labels,
        }
    }
}

impl<'a> ExprVisitor<SemanticResult<expr::Expr>> for SemanticAnalyzer<'a> {
    fn visit_assignment_expr(&mut self, expr: expr::AssignExpr) -> SemanticResult<expr::Expr> {
        Ok(expr::Expr::Assignment(expr))
    }

    fn visit_binary_expr(&mut self, expr: expr::BinaryExpr) -> SemanticResult<expr::Expr> {
        Ok(expr::Expr::Binary(expr))
    }

    fn visit_conditional_expr(
        &mut self,
        expr: expr::ConditionalExpr,
    ) -> SemanticResult<expr::Expr> {
        Ok(expr::Expr::Conditional(expr))
    }

    fn visit_constant(
        &mut self,
        expr: crate::with_token::WithToken<crate::lexer::token::Literal>,
    ) -> SemanticResult<expr::Expr> {
        Ok(expr::Expr::Constant(expr))
    }

    fn visit_unary_expr(&mut self, expr: expr::UnaryExpr) -> SemanticResult<expr::Expr> {
        Ok(expr::Expr::Unary(expr))
    }

    fn visit_variable(
        &mut self,
        var: crate::with_token::WithToken<crate::symbol::Symbol>,
    ) -> SemanticResult<expr::Expr> {
        Ok(expr::Expr::Variable(var))
    }
}

impl<'a> StmtVisitor<SemanticResult<stmt::Stmt>> for SemanticAnalyzer<'a> {
    fn visit_compound(&mut self, block: ast::Block) -> SemanticResult<stmt::Stmt> {
        Ok(stmt::Stmt::Compound(self.visit_block(block)?))
    }

    fn visit_expr_stmt(&mut self, stmt: expr::Expr) -> SemanticResult<stmt::Stmt> {
        Ok(stmt::Stmt::Expr(stmt))
    }

    fn visit_goto_stmt(
        &mut self,
        stmt: crate::with_token::WithToken<String>,
    ) -> SemanticResult<stmt::Stmt> {
        let label_name = &stmt.item;
        if let Some(_) = self.labels.get(label_name) {
            Ok(stmt::Stmt::Goto(stmt))
        } else {
            let token = stmt.get_token(self.source_file.get_tokens_checked());
            Err(SemanticAnalyzerError {
                err: SemanticError::UndefinedLabel(label_name.to_string()),
                span: (token.begin.0, token.end.0),
            })
        }
    }

    fn visit_if_stmt(&mut self, stmt: stmt::IfStmt) -> SemanticResult<stmt::Stmt> {
        Ok(stmt::Stmt::If(stmt))
    }

    fn visit_label_stmt(&mut self, stmt: stmt::Label) -> SemanticResult<stmt::Stmt> {
        Ok(stmt::Stmt::Label(stmt))
    }

    fn visit_null_stmt(&mut self) -> SemanticResult<stmt::Stmt> {
        Ok(stmt::Stmt::Null)
    }

    fn visit_return_stmt(
        &mut self,
        stmt: crate::with_token::WithToken<Option<expr::Expr>>,
    ) -> SemanticResult<stmt::Stmt> {
        Ok(stmt::Stmt::Return(stmt))
    }
}

impl<'a> ASTVisitor for SemanticAnalyzer<'a> {
    type BlockItemResult = SemanticResult<ast::BlockItem>;
    type BlockResult = SemanticResult<ast::Block>;
    type ExprResult = SemanticResult<expr::Expr>;
    type StmtResult = SemanticResult<stmt::Stmt>;
    type FunctionDefResult = SemanticResult<ast::FunctionDef>;
    type ProgramResult = SemanticResult<ast::Program>;
    type VarDeclResult = SemanticResult<ast::VarDeclaration>;

    fn visit_block_item(&mut self, item: ast::BlockItem) -> Self::BlockItemResult {
        match item {
            ast::BlockItem::Stmt(stmt) => Ok(ast::BlockItem::Stmt(self.visit_stmt(stmt)?)),
            ast::BlockItem::Decl(var_decl) => {
                Ok(ast::BlockItem::Decl(self.visit_var_decl(var_decl)?))
            }
        }
    }

    fn visit_block(&mut self, block: ast::Block) -> Self::BlockResult {
        Ok(ast::Block {
            block_begin: block.block_begin,
            body: block
                .body
                .into_iter()
                .map(|item| self.visit_block_item(item))
                .collect::<SemanticResult<_>>()?,
        })
    }

    fn visit_function_def(&mut self, func_def: ast::FunctionDef) -> Self::FunctionDefResult {
        let resolved_body = func_def
            .body
            .map(|block| self.visit_block(block))
            .transpose()?;
        Ok(ast::FunctionDef {
            name: func_def.name,
            body: resolved_body,
        })
    }

    fn visit_program(&mut self, program: ast::Program) -> Self::ProgramResult {
        let resolved_functions = program
            .function_defs
            .into_iter()
            .map(|func| self.visit_function_def(func))
            .collect::<SemanticResult<Vec<_>>>()?;
        Ok(ast::Program {
            function_defs: resolved_functions,
        })
    }

    fn visit_var_decl(&mut self, var_decl: ast::VarDeclaration) -> Self::VarDeclResult {
        Ok(var_decl)
    }
}
