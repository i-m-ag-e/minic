use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // punctuation
    Comma,
    Semicolon,
    Dot,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquare,
    RightSquare,

    // operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Equal,
    DoubleEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
    Not,
    LeftShift,
    RightShift,
    BitAnd,
    BitOr,
    BitXor,
    PlusAssign,
    MinusAssign,
    AsteriskAssign,
    SlashAssign,
    PercentAssign,
    LeftShiftAssign,
    RightShiftAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    Increment,
    Decrement,
    Arrow,

    // keywords
    KIf,
    KElse,
    KWhile,
    KFor,
    KReturn,

    // literals and identifiers
    Identifier(Symbol),
    Literal(Literal),
    Eof,
}
