use crate::{lexer::token::Token, with_token::WithToken};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Group(Box<WithToken<Expr>>),
    Unary(UnaryExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<WithToken<Expr>>,
    pub operator: Token,
    pub right: Box<WithToken<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub operand: Box<WithToken<Expr>>,
}

pub trait ExprVisitor<R> {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> R;
    fn visit_group_expr(&self, expr: &WithToken<Expr>) -> R;
    fn visit_unary_expr(&self, expr: &UnaryExpr) -> R;

    fn visit(&self, expr: &Expr) -> R {
        match expr {
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Group(group_expr) => self.visit_group_expr(group_expr),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
        }
    }
}

pub trait ExprVisitorMut<R> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> R;
    fn visit_group_expr(&mut self, expr: &WithToken<Expr>) -> R;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> R;

    fn visit(&mut self, expr: &Expr) -> R {
        match expr {
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Group(group_expr) => self.visit_group_expr(group_expr),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
        }
    }
}
