use crate::{
    ast::ASTRefVisitor,
    lexer::{Lexer, LexerResult},
    parser::Parser,
    source_file::SourceFile,
    symbol::SymbolTable,
    tacky::{Program, tacky_gen::TackyGen},
};
use anyhow;
use insta::assert_yaml_snapshot;

fn test_string_success(s: &str) -> anyhow::Result<Program> {
    let input = SourceFile::new("test_input.c".to_owned(), s.to_owned());
    let mut symbol_table = SymbolTable::new();
    let lexer = Lexer::new(&input, &mut symbol_table);
    let mut tokens = lexer.collect::<LexerResult<Vec<_>>>()?;
    let mut used_tokens = vec![false; tokens.len()];

    let mut parser = Parser::new(&tokens, &mut used_tokens);
    let prog = parser.parse()?;
    tokens = Parser::filter_saved_tokens(tokens, &mut used_tokens);

    let mut tacky_gen = TackyGen::new(&input);
    let tacky_prog = tacky_gen.visit_program(&prog);
    Ok(tacky_prog)
}

#[test]
fn test_simpl_return() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int main(void) { return 0; }")?);
    Ok(())
}

#[test]
fn test_unary_return() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int main(void) { return ~-42; }")?);
    Ok(())
}

#[test]
fn test_group_unary() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int main(void) { return -(-~-42); }")?);
    Ok(())
}
