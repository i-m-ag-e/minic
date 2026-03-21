use crate::{
    ast::{
        folder::ASTFolder,
        stmt::{self},
    },
    source_file::SourceFile,
    with_token::WithToken,
};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Undefined label: {0}")]
    UndefinedLabel(String),
}

#[derive(Error, Debug)]
#[error("Semantic error: {err}")]
pub struct SemanticAnalyzerError {
    pub err: SemanticError,
    pub span: (usize, usize),
}

#[derive(Debug)]
pub struct SemanticAnalyzer<'a> {
    source_file: &'a SourceFile,
    labels: HashMap<String, WithToken<()>>,
}

pub type SemanticResult<T> = Result<T, SemanticAnalyzerError>;

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(source_file: &'a SourceFile, labels: HashMap<String, WithToken<()>>) -> Self {
        Self {
            source_file,
            labels,
        }
    }
}

impl<'a> ASTFolder<SemanticAnalyzerError> for SemanticAnalyzer<'a> {
    fn visit_goto_stmt(
        &mut self,
        stmt: crate::with_token::WithToken<String>,
    ) -> SemanticResult<stmt::Stmt> {
        let label_name = &stmt.item;
        if let Some(_) = self.labels.get(label_name) {
            Ok(stmt::Stmt::Goto(stmt))
        } else {
            let token = stmt.get_token(self.source_file.get_tokens_checked());
            Err(SemanticAnalyzerError {
                err: SemanticError::UndefinedLabel(label_name.to_string()),
                span: (token.begin.0, token.end.0),
            })
        }
    }
}
