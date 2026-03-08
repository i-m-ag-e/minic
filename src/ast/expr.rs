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
    AddAssign,
    And,
    Assign,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    Divide,
    DivideAssign,
    Equal,
    Greater,
    GreaterEqual,
    LeftShift,
    LeftShiftAssign,
    LessEqual,
    LessThan,
    Modulus,
    ModulusAssign,
    Multiply,
    MultiplyAssign,
    NotEqual,
    Or,
    RightShift,
    RightShiftAssign,
    Subtract,
    SubtractAssign,
}

impl BinaryOp {
    pub fn precedence(&self) -> Option<u8> {
        match self {
            BinaryOp::Assign
            | BinaryOp::AddAssign
            | BinaryOp::BitOrAssign
            | BinaryOp::BitAndAssign
            | BinaryOp::BitXorAssign
            | BinaryOp::DivideAssign
            | BinaryOp::LeftShiftAssign
            | BinaryOp::ModulusAssign
            | BinaryOp::MultiplyAssign
            | BinaryOp::RightShiftAssign
            | BinaryOp::SubtractAssign => Some(50),
            BinaryOp::Or => Some(60),
            BinaryOp::And => Some(65),
            BinaryOp::BitOr => Some(70),
            BinaryOp::BitXor => Some(75),
            BinaryOp::BitAnd => Some(80),
            BinaryOp::Equal => Some(85),
            BinaryOp::NotEqual => Some(85),
            BinaryOp::Greater => Some(90),
            BinaryOp::GreaterEqual => Some(90),
            BinaryOp::LessEqual => Some(90),
            BinaryOp::LessThan => Some(90),
            BinaryOp::LeftShift => Some(95),
            BinaryOp::RightShift => Some(95),
            BinaryOp::Add => Some(100),
            BinaryOp::Subtract => Some(100),
            BinaryOp::Divide => Some(105),
            BinaryOp::Modulus => Some(105),
            BinaryOp::Multiply => Some(105),
        }
    }

    pub fn from_compound_assign(token_type: &TokenType) -> Option<Self> {
        match token_type {
            TokenType::PlusAssign => Some(BinaryOp::Add),
            TokenType::BitAndAssign => Some(BinaryOp::BitAnd),
            TokenType::BitOrAssign => Some(BinaryOp::BitOr),
            TokenType::BitXorAssign => Some(BinaryOp::BitXor),
            TokenType::SlashAssign => Some(BinaryOp::Divide),
            TokenType::LeftShiftAssign => Some(BinaryOp::LeftShift),
            TokenType::RightShiftAssign => Some(BinaryOp::RightShift),
            TokenType::MinusAssign => Some(BinaryOp::Subtract),
            TokenType::AsteriskAssign => Some(BinaryOp::Multiply),
            TokenType::PercentAssign => Some(BinaryOp::Modulus),
            _ => None,
        }
    }
}

impl TryFrom<&TokenType> for BinaryOp {
    type Error = ();

    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Plus => Ok(BinaryOp::Add),
            TokenType::PlusAssign => Ok(BinaryOp::AddAssign),
            TokenType::And => Ok(BinaryOp::And),
            TokenType::Equal => Ok(BinaryOp::Assign),
            TokenType::BitAnd => Ok(BinaryOp::BitAnd),
            TokenType::BitAndAssign => Ok(BinaryOp::BitAndAssign),
            TokenType::BitOr => Ok(BinaryOp::BitOr),
            TokenType::BitOrAssign => Ok(BinaryOp::BitOrAssign),
            TokenType::BitXor => Ok(BinaryOp::BitXor),
            TokenType::BitXorAssign => Ok(BinaryOp::BitXorAssign),
            TokenType::Slash => Ok(BinaryOp::Divide),
            TokenType::SlashAssign => Ok(BinaryOp::DivideAssign),
            TokenType::DoubleEqual => Ok(BinaryOp::Equal),
            TokenType::Greater => Ok(BinaryOp::Greater),
            TokenType::GreaterEqual => Ok(BinaryOp::GreaterEqual),
            TokenType::LeftShift => Ok(BinaryOp::LeftShift),
            TokenType::LeftShiftAssign => Ok(BinaryOp::LeftShiftAssign),
            TokenType::LessEqual => Ok(BinaryOp::LessEqual),
            TokenType::Less => Ok(BinaryOp::LessThan),
            TokenType::Percent => Ok(BinaryOp::Modulus),
            TokenType::PercentAssign => Ok(BinaryOp::ModulusAssign),
            TokenType::Asterisk => Ok(BinaryOp::Multiply),
            TokenType::AsteriskAssign => Ok(BinaryOp::MultiplyAssign),
            TokenType::NotEqual => Ok(BinaryOp::NotEqual),
            TokenType::Or => Ok(BinaryOp::Or),
            TokenType::RightShift => Ok(BinaryOp::RightShift),
            TokenType::RightShiftAssign => Ok(BinaryOp::RightShiftAssign),
            TokenType::Minus => Ok(BinaryOp::Subtract),
            TokenType::MinusAssign => Ok(BinaryOp::SubtractAssign),
            _ => Err(()),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            BinaryOp::Add => "+",
            BinaryOp::AddAssign => "+=",
            BinaryOp::And => "&&",
            BinaryOp::Assign => "=",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitAndAssign => "&=",
            BinaryOp::BitOr => "|",
            BinaryOp::BitOrAssign => "|=",
            BinaryOp::BitXor => "^",
            BinaryOp::BitXorAssign => "^=",
            BinaryOp::Divide => "/",
            BinaryOp::DivideAssign => "/=",
            BinaryOp::Equal => "==",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::LeftShift => "<<",
            BinaryOp::LeftShiftAssign => "<<=",
            BinaryOp::LessEqual => "<=",
            BinaryOp::LessThan => "<",
            BinaryOp::Modulus => "%",
            BinaryOp::ModulusAssign => "%=",
            BinaryOp::Multiply => "*",
            BinaryOp::MultiplyAssign => "*=",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Or => "||",
            BinaryOp::RightShift => ">>",
            BinaryOp::RightShiftAssign => ">>=",
            BinaryOp::Subtract => "-",
            BinaryOp::SubtractAssign => "-=",
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
    BitNot,
    Decrement,
    Increment,
    Negate,
    Not,
}

impl TryFrom<&TokenType> for UnaryOp {
    type Error = ();

    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::BitNot => Ok(UnaryOp::BitNot),
            TokenType::Decrement => Ok(UnaryOp::Decrement),
            TokenType::Increment => Ok(UnaryOp::Increment),
            TokenType::Minus => Ok(UnaryOp::Negate),
            TokenType::Not => Ok(UnaryOp::Not),
            _ => Err(()),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            UnaryOp::BitNot => "~",
            UnaryOp::Decrement => "--",
            UnaryOp::Increment => "++",
            UnaryOp::Negate => "-",
            UnaryOp::Not => "!",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: WithToken<UnaryOp>,
    pub operand: Box<Expr>,
    pub postfix: bool,
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
