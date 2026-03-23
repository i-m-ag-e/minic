use crate::{
    ast::{ASTVisitor, folder::ASTFolder},
    lexer::{Lexer, LexerResult},
    parser::Parser,
    resolver::Resolver,
    semantic_analyzer::SemanticAnalyzer,
    source_file::SourceFile,
    symbol::SymbolTable,
    tacky::{Program, tacky_gen::TackyGen},
};
use anyhow;
use insta::assert_yaml_snapshot;

fn test_string_success(s: &str) -> anyhow::Result<Program> {
    let mut input = SourceFile::new("test_input.c".to_owned(), s.to_owned());
    let mut symbol_table = SymbolTable::new();
    let lexer = Lexer::new(&input, &mut symbol_table);
    let tokens = lexer.collect::<LexerResult<Vec<_>>>()?;
    let mut used_tokens = vec![false; tokens.len()];

    let mut parser = Parser::new(&tokens, &mut used_tokens, &symbol_table);
    let prog = parser.parse()?;
    input.set_tokens(Parser::filter_saved_tokens(tokens, &mut used_tokens));

    let mut resolver = Resolver::new(symbol_table, &input);
    let prog = resolver.visit_program(prog)?;
    let (var_map, labels) = resolver.release_symbol_table_and_labels();

    let mut analyzer = SemanticAnalyzer::new(&input, labels);
    let prog = analyzer.visit_program(prog)?;
    let switch_map = analyzer.release_switch_map();

    let mut tacky_gen = TackyGen::new(&input, switch_map, &var_map);
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

#[test]
fn test_binary() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int main(void) { return 1 + 2 * 3; }")?);
    Ok(())
}

#[test]
fn test_logic() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success(
        "int main(void) { return 1 && 0 || 1; }"
    )?);
    Ok(())
}

#[test]
fn test_logic_short_circuit() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success(
        "int main(void) { return 1 || (1 / 0); }"
    )?);
    Ok(())
}

#[test]
fn test_comparison() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success(
        "int main(void) { return 1 < 2 && 3 >= 4; }"
    )?);
    Ok(())
}

#[test]
fn test_comparison_equality() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success(
        "int main(void) { return 1 == 1 && 2 != 3; }"
    )?);
    Ok(())
}
