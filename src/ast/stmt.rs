use crate::with_token::WithToken;

use super::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Null,
    Return(WithToken<Option<Expr>>),
}

pub trait StmtVisitor<R> {
    fn visit_expr_stmt(&mut self, stmt: Expr) -> R;
    fn visit_null_stmt(&mut self) -> R;
    fn visit_return_stmt(&mut self, stmt: WithToken<Option<Expr>>) -> R;

    fn visit_stmt(&mut self, stmt: Stmt) -> R {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Null => self.visit_null_stmt(),
            Stmt::Return(ret) => self.visit_return_stmt(ret),
        }
    }
}

pub trait StmtRefVisitor<R> {
    fn visit_expr_stmt(&mut self, stmt: &Expr) -> R;
    fn visit_null_stmt(&mut self) -> R;
    fn visit_return_stmt(&mut self, stmt: &WithToken<Option<Expr>>) -> R;

    fn visit_stmt(&mut self, stmt: &Stmt) -> R {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Null => self.visit_null_stmt(),
            Stmt::Return(ret) => self.visit_return_stmt(ret),
        }
    }
}
