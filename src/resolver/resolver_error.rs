use thiserror::Error;

use crate::source_file::SourcePosition;

#[derive(Debug, Clone, Error)]
pub enum ResolverErrorType {
    #[error("Duplicate label definition: previous declaration at {prev_line}:{prev_col}")]
    DuplicateLabel {
        label: String,
        prev_line: usize,
        prev_col: usize,
    },
    #[error("Invalid assignment target")]
    InvalidLValue,
    #[error(
        "`{name}` redeclared as a different kind (previously declared as {prev_kind}, now as {new_kind}; previous declaration at {prev_line}:{prev_col})"
    )]
    RedeclaredAsDifferentKind {
        name: String,
        new_kind: &'static str,
        prev_kind: &'static str,
        prev_line: usize,
        prev_col: usize,
    },
    #[error(
        "More than one definitions for function `{name}`  (previous defintion at {prev_line}:{prev_col})"
    )]
    RedefinedFunction {
        name: String,
        prev_line: usize,
        prev_col: usize,
    },
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
