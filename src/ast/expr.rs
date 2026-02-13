use std::fmt::Display;

use serde::Serialize;

use crate::{
    lexer::token::{Literal, TokenID},
    with_token::WithToken,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Constant(WithToken<Literal>),
    Unary(UnaryExpr),
}

impl Expr {
    pub fn token(&self) -> TokenID {
        match self {
            Expr::Binary(binary_expr) => binary_expr.operator.token_id,
            Expr::Constant(lit) => lit.token_id,
            Expr::Unary(unary_expr) => unary_expr.operator.token_id,
        }
    }
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

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            BinaryOp::Add => "+",
            BinaryOp::And => "&&",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Divide => "/",
            BinaryOp::Equal => "==",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::LeftShift => "<<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::LessThan => "<",
            BinaryOp::Modulus => "%",
            BinaryOp::Multiply => "*",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Or => "||",
            BinaryOp::RightShift => ">>",
            BinaryOp::Subtract => "-",
        };
        write!(f, "{}", op_str)
    }
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

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            UnaryOp::Negate => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "~",
        };
        write!(f, "{}", op_str)
    }
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
