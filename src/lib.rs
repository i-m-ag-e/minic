mod lexer;
pub mod source_file;
mod symbol;

use anyhow;
use lexer::Lexer;
use source_file::SourceFile;
use symbol::SymbolTable;

pub use lexer::lexer_error;

pub fn compile(source_file: &SourceFile) -> anyhow::Result<()> {
    let mut interner = SymbolTable::new();
    let lexer = Lexer::new(source_file, &mut interner);
    for token in lexer {
        println!("{:#?}", token?);
    }
    Ok(())
}
