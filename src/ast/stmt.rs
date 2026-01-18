use super::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
}

pub trait StmtVisitor<R> {
    fn visit_expr_stmt(&self, stmt: &Expr) -> R;

    fn visit(&self, stmt: &Stmt) -> R {
        match stmt {
            Stmt::ExprStmt(expr) => self.visit_expr_stmt(expr),
        }
    }
}

pub trait StmtVisitorMut<R> {
    fn visit_expr_stmt(&mut self, stmt: &Expr) -> R;

    fn visit(&mut self, stmt: &Stmt) -> R {
        match stmt {
            Stmt::ExprStmt(expr) => self.visit_expr_stmt(expr),
        }
    }
}
