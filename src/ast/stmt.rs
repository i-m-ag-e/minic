use super::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Return(Option<Expr>),
}

pub trait StmtVisitor<R> {
    fn visit_expr_stmt(&mut self, stmt: Expr) -> R;
    fn visit_return_stmt(&mut self, stmt: Option<Expr>) -> R;

    fn visit_stmt(&mut self, stmt: Stmt) -> R {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Return(ret) => self.visit_return_stmt(ret),
        }
    }
}

pub trait StmtRefVisitor<R> {
    fn visit_expr_stmt(&mut self, stmt: &Expr) -> R;
    fn visit_return_stmt(&mut self, stmt: &Option<Expr>) -> R;

    fn visit_stmt(&mut self, stmt: &Stmt) -> R {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Return(ret) => self.visit_return_stmt(ret),
        }
    }
}
