use std::ops::Deref;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    filename: String,
    content: String,
}

impl SourceFile {
    pub fn new(filename: String, content: String) -> Self {
        Self { filename, content }
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }
}

impl Deref for SourceFile {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourcePosition {
    pub pos: usize,
    pub line: usize,
    pub column: usize,
}

impl SourcePosition {
    pub fn new(pos: usize, line: usize, column: usize) -> Self {
        SourcePosition { pos, line, column }
    }

    pub fn increment(&self, c: char) -> Self {
        if c == '\n' {
            SourcePosition {
                pos: self.pos + 1,
                line: self.line + 1,
                column: 1,
            }
        } else {
            SourcePosition {
                pos: self.pos + 1,
                line: self.line,
                column: self.column + 1,
            }
        }
    }
}
