use crate::{source_file::SourcePosition, symbol::Symbol};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    // punctuation
    Colon,
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
    QuestionMark,
    RightShift,
    RightShiftAssign,
    Slash,
    SlashAssign,

    // keywords
    KBreak,
    KCase,
    KContinue,
    KDefault,
    KDo,
    KElse,
    KFor,
    KGoto,
    KIf,
    KInt,
    KReturn,
    KSwitch,
    KVoid,
    KWhile,

    // literals and identifiers
    Identifier(Symbol),
    Literal(Literal),
    Eof,
}

pub type TokenID = usize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub begin: SourcePosition,
    pub end: SourcePosition,
}
impl Token {
    pub fn span(&self) -> (SourcePosition, SourcePosition) {
        (self.begin, self.end)
    }
}
