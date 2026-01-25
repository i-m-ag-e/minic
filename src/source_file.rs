use std::ops::{Add, Deref, Sub};

use serde::Serialize;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub filename: String,
    pub content: String,
    pub line_starts: Vec<usize>,
}

impl SourceFile {
    pub fn new(filename: String, content: String) -> Self {
        let mut pos = 0;
        let mut line_starts = vec![0];
        for line in content.lines() {
            pos += line.len() + 1;
            line_starts.push(pos);
        }
        line_starts.push(content.len());
        Self {
            filename,
            content,
            line_starts,
        }
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn line_col(&self, pos: usize) -> (usize, usize) {
        let mut line = 0;
        for (i, &start) in self.line_starts.iter().enumerate() {
            if start > pos {
                break;
            }
            line = i + 1;
        }
        let col = pos - self.line_starts[line - 1] + 1;
        (line, col)
    }
}

impl Deref for SourceFile {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SourcePosition(pub usize);

impl Add<usize> for SourcePosition {
    type Output = SourcePosition;

    fn add(self, rhs: usize) -> Self::Output {
        SourcePosition(self.0 + rhs)
    }
}

impl Sub<usize> for SourcePosition {
    type Output = SourcePosition;

    fn sub(self, rhs: usize) -> Self::Output {
        SourcePosition(self.0 - rhs)
    }
}

impl Deref for SourcePosition {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_col() {
        let source = SourceFile::new(
            "test.c".to_string(),
            "int main() {\n    return 0; }".to_string(),
        );
        assert_eq!(source.line_col(0), (1, 1));
        assert_eq!(source.line_col(4), (1, 5));
        assert_eq!(source.line_col(12), (1, 13));
        assert_eq!(source.line_col(13), (2, 1));
        assert_eq!(source.line_col(18), (2, 6));
        assert_eq!(source.line_col(21), (2, 9));
    }

    #[test]
    fn test_multi_newline() {
        let source = SourceFile::new(
            "test.c".to_string(),
            "line1\nline2\n\nline4\n\n\nline5".to_string(),
        );
        assert_eq!(source.line_col(0), (1, 1));
        assert_eq!(source.line_col(6), (2, 1));
        assert_eq!(source.line_col(12), (3, 1));
        assert_eq!(source.line_col(13), (4, 1));
        assert_eq!(source.line_col(18), (4, 6));
        assert_eq!(source.line_col(19), (5, 1));
        assert_eq!(source.line_col(20), (6, 1));
        assert_eq!(source.line_col(21), (7, 1));
    }
}
