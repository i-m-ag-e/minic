use thiserror::Error;

#[derive(Debug, Error, Clone, Copy)]
pub enum LexerErrorType {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),
    #[error("Expected character '{expected}', but found '{found}'")]
    ExpectedAnother { expected: char, found: char },
    #[error("Unterminated string literal")]
    UnterminatedString,
    #[error("Invalid number format")]
    InvalidNumberFormat,
}

#[derive(Debug, Error, Clone, Copy)]
#[error("Syntax error at {line}:{column}: {err_type}")]
pub struct LexerError {
    pub line: usize,
    pub column: usize,
    pub err_type: LexerErrorType,
}

impl LexerError {
    pub fn new(line: usize, column: usize, err_type: LexerErrorType) -> Self {
        LexerError {
            line,
            column,
            err_type,
        }
    }
}
