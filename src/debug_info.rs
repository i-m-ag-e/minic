use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct DebugInfo {
    pub source_line: usize,
    pub source_col: usize,
    pub message: String,
}

impl DebugInfo {
    pub fn new(source_line: usize, source_col: usize, message: String) -> Self {
        Self {
            source_line,
            source_col,
            message,
        }
    }

    pub fn with_additional(&self, additional_message: String) -> Self {
        Self {
            source_line: self.source_line,
            source_col: self.source_col,
            message: format!("({}) {}", self.message, additional_message),
        }
    }
}

impl Display for DebugInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}:{}) :: {}",
            self.source_line, self.source_col, self.message
        )
    }
}
