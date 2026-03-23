use crate::{
    ast::{
        expr,
        folder::ASTFolder,
        stmt::{self, LoopID},
    },
    lexer::token::Literal,
    source_file::{SourceFile, SourcePosition},
    with_token::WithToken,
};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Default, Clone)]
pub struct SwitchData {
    pub cases: Vec<WithToken<Literal>>,
    pub default: Option<WithToken<()>>,
}

pub type SwitchMap = HashMap<LoopID, SwitchData>;

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("`case` used outside a switch statement")]
    CaseOutsideLoop,
    #[error("`default` used outside a switch statement")]
    DefaultOutsideLoop,
    #[error("duplicate case label (previous case with same value at {prev_line}:{prev_col}")]
    DuplicateCaseLabel { prev_line: usize, prev_col: usize },
    #[error(
        "`default` used twice in the same switch statement; previous use at {prev_line}:{prev_col}"
    )]
    DuplicateDefault { prev_line: usize, prev_col: usize },
    #[error("non-constant and non-literal values are not allowed in case statements")]
    NonConstCase,
    #[error("Undefined label: {0}")]
    UndefinedLabel(String),
}

#[derive(Error, Debug)]
#[error("Semantic error: {err}")]
pub struct SemanticAnalyzerError {
    pub err: SemanticError,
    pub span: (SourcePosition, SourcePosition),
}

#[derive(Debug)]
pub struct SemanticAnalyzer<'a> {
    source_file: &'a SourceFile,
    labels: HashMap<String, WithToken<()>>,
    switch_map: SwitchMap,
    switch_stack: Vec<LoopID>,
}

pub type SemanticResult<T> = Result<T, SemanticAnalyzerError>;

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(source_file: &'a SourceFile, labels: HashMap<String, WithToken<()>>) -> Self {
        Self {
            source_file,
            labels,
            switch_map: SwitchMap::new(),
            switch_stack: Vec::new(),
        }
    }

    pub fn release_switch_map(self) -> SwitchMap {
        self.switch_map
    }
}

impl<'a> ASTFolder<SemanticAnalyzerError> for SemanticAnalyzer<'a> {
    fn visit_case_stmt(
        &mut self,
        stmt: stmt::CaseStmt,
    ) -> Result<stmt::Stmt, SemanticAnalyzerError> {
        if let Some(switch_data) = self
            .switch_stack
            .last()
            .copied()
            .and_then(|id| self.switch_map.get_mut(&id))
        {
            let with_token = stmt.value.with_value(());
            let expr::Expr::Constant(WithToken {
                item: Literal::Integer(lit),
                ..
            }) = *stmt.value
            else {
                let token = stmt.value.get_token(self.source_file.get_tokens_checked());
                return Err(SemanticAnalyzerError {
                    err: SemanticError::NonConstCase,
                    span: token.span(),
                });
            };

            if let Some(prev) = switch_data
                .cases
                .iter()
                .find(|case| case.item == Literal::Integer(lit))
            {
                let token = prev.get_token(self.source_file.get_tokens_checked());
                let (prev_line, prev_col) = self.source_file.line_col(token.begin.0);
                return Err(SemanticAnalyzerError {
                    err: SemanticError::DuplicateCaseLabel {
                        prev_line,
                        prev_col,
                    },
                    span: token.span(),
                });
            }

            switch_data
                .cases
                .push(with_token.with_value(Literal::Integer(lit)));

            self.fold_case_stmt(stmt)
        } else {
            let token = stmt.value.get_token(self.source_file.get_tokens_checked());
            Err(SemanticAnalyzerError {
                err: SemanticError::CaseOutsideLoop,
                span: token.span(),
            })
        }
    }

    fn visit_default_stmt(
        &mut self,
        stmt: stmt::DefaultStmt,
    ) -> Result<stmt::Stmt, SemanticAnalyzerError> {
        if let Some(switch_data) = self
            .switch_stack
            .last()
            .copied()
            .and_then(|id| self.switch_map.get_mut(&id))
        {
            if let Some(prev) = switch_data.default {
                let token = prev.get_token(self.source_file.get_tokens_checked());
                let (prev_line, prev_col) = self.source_file.line_col(token.begin.0);
                return Err(SemanticAnalyzerError {
                    err: SemanticError::DuplicateDefault {
                        prev_line,
                        prev_col,
                    },
                    span: token.span(),
                });
            }

            switch_data.default = Some(stmt.default_token);

            self.fold_default_stmt(stmt)
        } else {
            let token = stmt
                .default_token
                .get_token(self.source_file.get_tokens_checked());
            Err(SemanticAnalyzerError {
                err: SemanticError::DefaultOutsideLoop,
                span: token.span(),
            })
        }
    }

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
                span: token.span(),
            })
        }
    }

    fn visit_switch_stmt(
        &mut self,
        stmt: stmt::SwitchStmt,
    ) -> Result<stmt::Stmt, SemanticAnalyzerError> {
        self.switch_stack.push(stmt.switch_id);
        self.switch_map
            .insert(stmt.switch_id, SwitchData::default());
        let result = self.fold_switch_stmt(stmt)?;
        self.switch_stack
            .pop()
            .expect("switch stack cannot be empty while popping");

        Ok(result)
    }
}
