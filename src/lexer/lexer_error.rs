use thiserror::Error;

use crate::source_file::SourcePosition;

#[derive(Debug, Error, Clone, Copy)]
pub enum LexerErrorType {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),
    #[error("Expected character '{expected}', but found '{found}'")]
    ExpectedAnother { expected: char, found: char },
    #[error("Unterminated string literal")]
    UnterminatedString,
    #[error("Invalid number literal")]
    InvalidNumberLiteral,
}

#[derive(Debug, Error, Clone)]
#[error("Syntax error at {}:{} :: {}", range.0.line, range.0.column, err_type)]
pub struct LexerError {
    pub range: (SourcePosition, SourcePosition),
    pub err_type: LexerErrorType,
}

impl LexerError {
    pub fn new(start: SourcePosition, end: SourcePosition, err_type: LexerErrorType) -> Self {
        LexerError {
            range: (start, end),
            err_type,
        }
    }
}
