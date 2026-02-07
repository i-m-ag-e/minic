use serde::Serialize;

use crate::{
    lexer::token::{Literal, Token},
    with_token::WithToken,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Constant(WithToken<Literal>),
    Unary(UnaryExpr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub enum BinaryOp {
    Add,
    And,
    BitAnd,
    BitOr,
    BitXor,
    Divide,
    Equal,
    Greater,
    GreaterEqual,
    LeftShift,
    LessEqual,
    LessThan,
    Modulus,
    Multiply,
    NotEqual,
    Or,
    RightShift,
    Subtract,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: WithToken<BinaryOp>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum UnaryOp {
    Negate,
    Not,
    BitNot,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: WithToken<UnaryOp>,
    pub operand: Box<Expr>,
}

pub trait ExprVisitor<R> {
    fn visit_binary_expr(&mut self, expr: BinaryExpr) -> R;
    fn visit_constant(&mut self, expr: WithToken<Literal>) -> R;
    fn visit_unary_expr(&mut self, expr: UnaryExpr) -> R;

    fn visit_expr(&mut self, expr: Expr) -> R {
        match expr {
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Constant(lit) => self.visit_constant(lit),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
        }
    }
}

pub trait ExprRefVisitor<R> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> R;
    fn visit_constant(&mut self, expr: &WithToken<Literal>) -> R;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> R;

    fn visit_expr(&mut self, expr: &Expr) -> R {
        match expr {
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Constant(lit) => self.visit_constant(lit),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
        }
    }
}
