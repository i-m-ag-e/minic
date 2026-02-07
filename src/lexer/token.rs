use crate::{source_file::SourcePosition, symbol::Symbol};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // punctuation
    Comma,
    Dot,
    LeftBrace,
    LeftParen,
    LeftSquare,
    RightBrace,
    RightParen,
    RightSquare,
    Semicolon,

    // operators
    And,
    Arrow,
    Asterisk,
    AsteriskAssign,
    BitAnd,
    BitAndAssign,
    BitNot,
    BitNotAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    Decrement,
    DoubleEqual,
    Equal,
    Greater,
    GreaterEqual,
    Increment,
    LeftShift,
    LeftShiftAssign,
    Less,
    LessEqual,
    Minus,
    MinusAssign,
    Not,
    NotEqual,
    Or,
    Percent,
    PercentAssign,
    Plus,
    PlusAssign,
    RightShift,
    RightShiftAssign,
    Slash,
    SlashAssign,

    // keywords
    KElse,
    KFor,
    KIf,
    KInt,
    KReturn,
    KVoid,
    KWhile,

    // literals and identifiers
    Identifier(Symbol),
    Literal(Literal),
    Eof,
}

pub type TokenID = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub begin: SourcePosition,
    pub end: SourcePosition,
}
