use thiserror::Error;

use crate::{source_file::SourcePosition, type_checker::r#type::Type};

#[derive(Debug, Clone, Error)]
pub enum TypeCheckerErrorType {
    #[error("function called with wrong number of arguments: expected {expected}, got {actual}")]
    FunctionCallWrongArgsCount { expected: usize, actual: usize },

    #[error(
        "function redeclared with a different type `{new_type}` (previously declared as `{prev_type}` at {0}:{1})", prev_line_col.0, prev_line_col.1
    )]
    FunctionConflictingDeclarations {
        new_type: Type,
        prev_type: Type,
        prev_line_col: (usize, usize),
    },

    #[error("argument {arg_index} has incorrect type: expected `{expected}`, got `{actual}`")]
    IncorrectArgumentType {
        expected: Type,
        actual: Type,
        arg_index: usize,
    },

    #[error("type `{0}` cannot be called")]
    TypeNotCallable(Type),

    #[error("type `{0}` cannot be used as an lvalue")]
    TypeCannotBeLValue(Type),

    #[error("type mismatch: expected `{expected}`, got `{actual}`")]
    TypeMismatch { expected: Type, actual: Type },
}

#[derive(Debug, Clone, Error)]
#[error("Type checker error: {error_type}")]
pub struct TypeCheckerError {
    pub error_type: TypeCheckerErrorType,
    pub span: (SourcePosition, SourcePosition),
}
