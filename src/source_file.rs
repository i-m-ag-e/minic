use std::ops::Deref;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    filename: String,
    content: String,
}

impl Deref for SourceFile {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}
