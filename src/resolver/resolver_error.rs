use thiserror::Error;

use crate::source_file::SourcePosition;

#[derive(Debug, Clone, Error)]
pub enum ResolverErrorType {
    #[error("Invalid assignment target")]
    InvalidAssignmentTarget,
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("Variable already defined: previous declaration at {prev_line}:{prev_col}")]
    VariableAlreadyDefined {
        name: String,
        prev_col: usize,
        prev_line: usize,
    },
    #[error("'return' statement outside of function")]
    ReturnOutsideFunction,
}

#[derive(Debug, Error)]
#[error("Resolver error: {err_type}")]
pub struct ResolverError {
    pub err_type: ResolverErrorType,
    pub span: (SourcePosition, SourcePosition),
}

#[derive(Debug, Clone, Error)]
pub enum ResolverWarningType {
    #[error("non-void function {0} has no return statement")]
    NonVoidFunctionNoReturn(String),
}

#[derive(Debug, Error)]
#[error("Resolver warning: {warn_type}")]
pub struct ResolverWarning {
    pub warn_type: ResolverWarningType,
    pub location: (SourcePosition, SourcePosition),
}
