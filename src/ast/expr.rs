use std::fmt::Display;

use serde::Serialize;

use crate::{
    lexer::token::{Literal, TokenID, TokenType},
    symbol::Symbol,
    with_token::WithToken,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assignment(AssignExpr),
    Binary(BinaryExpr),
    Constant(WithToken<Literal>),
    Unary(UnaryExpr),
    Variable(WithToken<Symbol>),
}

impl Expr {
    pub fn token(&self) -> TokenID {
        match self {
            Expr::Assignment(assign) => assign.eq_token.token_id,
            Expr::Binary(binary_expr) => binary_expr.operator.token_id,
            Expr::Constant(lit) => lit.token_id,
            Expr::Unary(unary_expr) => unary_expr.operator.token_id,
            Expr::Variable(var) => var.token_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub target: Box<Expr>,
    pub eq_token: WithToken<()>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub enum BinaryOp {
    Add,
    And,
    Assign,
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

impl TryFrom<&TokenType> for BinaryOp {
    type Error = ();

    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Plus => Ok(BinaryOp::Add),
            TokenType::And => Ok(BinaryOp::And),
            TokenType::Equal => Ok(BinaryOp::Assign),
            TokenType::BitAnd => Ok(BinaryOp::BitAnd),
            TokenType::BitOr => Ok(BinaryOp::BitOr),
            TokenType::BitXor => Ok(BinaryOp::BitXor),
            TokenType::Slash => Ok(BinaryOp::Divide),
            TokenType::DoubleEqual => Ok(BinaryOp::Equal),
            TokenType::Greater => Ok(BinaryOp::Greater),
            TokenType::GreaterEqual => Ok(BinaryOp::GreaterEqual),
            TokenType::LeftShift => Ok(BinaryOp::LeftShift),
            TokenType::LessEqual => Ok(BinaryOp::LessEqual),
            TokenType::Less => Ok(BinaryOp::LessThan),
            TokenType::Percent => Ok(BinaryOp::Modulus),
            TokenType::Asterisk => Ok(BinaryOp::Multiply),
            TokenType::NotEqual => Ok(BinaryOp::NotEqual),
            TokenType::Or => Ok(BinaryOp::Or),
            TokenType::RightShift => Ok(BinaryOp::RightShift),
            TokenType::Minus => Ok(BinaryOp::Subtract),
            _ => Err(()),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            BinaryOp::Add => "+",
            BinaryOp::And => "&&",
            BinaryOp::Assign => "=",
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

impl TryFrom<&TokenType> for UnaryOp {
    type Error = ();

    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Minus => Ok(UnaryOp::Negate),
            TokenType::Not => Ok(UnaryOp::Not),
            TokenType::BitNot => Ok(UnaryOp::BitNot),
            _ => Err(()),
        }
    }
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
    fn visit_assignment_expr(&mut self, expr: AssignExpr) -> R;
    fn visit_binary_expr(&mut self, expr: BinaryExpr) -> R;
    fn visit_constant(&mut self, expr: WithToken<Literal>) -> R;
    fn visit_unary_expr(&mut self, expr: UnaryExpr) -> R;
    fn visit_variable(&mut self, var: WithToken<Symbol>) -> R;

    fn visit_expr(&mut self, expr: Expr) -> R {
        match expr {
            Expr::Assignment(assign) => self.visit_assignment_expr(assign),
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Constant(lit) => self.visit_constant(lit),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
            Expr::Variable(var) => self.visit_variable(var),
        }
    }
}

pub trait ExprRefVisitor<R> {
    fn visit_assignment_expr(&mut self, expr: &AssignExpr) -> R;
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> R;
    fn visit_constant(&mut self, expr: &WithToken<Literal>) -> R;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> R;
    fn visit_variable(&mut self, var: &WithToken<Symbol>) -> R;

    fn visit_expr(&mut self, expr: &Expr) -> R {
        match expr {
            Expr::Assignment(assign) => self.visit_assignment_expr(assign),
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Constant(lit) => self.visit_constant(lit),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
            Expr::Variable(var) => self.visit_variable(var),
        }
    }
}
