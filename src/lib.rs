mod ast;
mod lexer;
pub mod source_file;
mod symbol;
mod with_token;

use anyhow;
use lexer::Lexer;
use source_file::SourceFile;
use symbol::SymbolTable;

pub use lexer::lexer_error;

use crate::lexer::{LexerResult, token::TokenType};

pub fn compile(source_file: &SourceFile) -> anyhow::Result<()> {
    let mut interner = SymbolTable::new();
    let lexer = Lexer::new(source_file, &mut interner);
    let tokens: Vec<_> = lexer.collect::<LexerResult<Vec<_>>>()?;
    for token in tokens {
        let lexeme = &source_file[token.begin.0..token.end.0];
        let line_col_start = source_file.line_col(token.begin.0);
        let line_col_end = source_file.line_col(token.end.0);
        print!(
            "{:#?} <{:?} :: ({}:{} - {}:{}>)",
            token, lexeme, line_col_start.0, line_col_start.1, line_col_end.0, line_col_end.1
        );
        if let TokenType::Identifier(sym) = &token.token_type {
            let name = interner.resolve(*sym).unwrap();
            println!(" (ident: {:?})", name);
        } else {
            println!("");
        }
    }
    Ok(())
}
