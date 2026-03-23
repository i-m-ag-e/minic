use crate::lexer::token::TokenType;
use crate::source_file::SourcePosition;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ParserErrorType {
    #[error("`break` statement outside of a loop or switch statement")]
    BreakOutsideLoopOrSwitch,
    #[error("`continue` statement outside of a loop")]
    ContinueOutsideLoop,

    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(TokenType),
    #[error("Expected token: {expected:?}, but found: {found:?}")]
    ExpectedAnother {
        expected: TokenType,
        found: TokenType,
    },
    #[error("Expected token: {expected}, but found: {found:?}")]
    ExpectedAnotherString {
        expected: &'static str,
        found: TokenType,
    },
}

#[derive(Debug, Error)]
#[error("Parse error: {err_type}")]
pub struct ParserError {
    pub err_type: ParserErrorType,
    pub span: (SourcePosition, SourcePosition),
}
