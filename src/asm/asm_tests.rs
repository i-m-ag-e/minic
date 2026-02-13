use crate::{
    asm::tacky_to_asm,
    ast::ASTRefVisitor,
    lexer::{Lexer, LexerResult},
    parser::Parser,
    source_file::SourceFile,
    symbol::SymbolTable,
    tacky::tacky_gen::TackyGen,
};
use anyhow;

fn compile_to_asm_string(s: &str) -> anyhow::Result<String> {
    let mut input = SourceFile::new("test_input.c".to_owned(), s.to_owned());
    let mut symbol_table = SymbolTable::new();

    let lexer = Lexer::new(&input, &mut symbol_table);
    let tokens = lexer.collect::<LexerResult<Vec<_>>>()?;

    let mut used_tokens = vec![false; tokens.len()];
    let mut parser = Parser::new(&tokens, &mut used_tokens);
    let ast_program = parser.parse()?;
    input.set_tokens(Parser::filter_saved_tokens(tokens, &mut used_tokens));

    let mut tacky_gen = TackyGen::new(&input);
    let tacky_prog = tacky_gen.visit_program(&ast_program);

    let asm_ast = tacky_to_asm(&tacky_prog);

    let mut asm_str = String::new();
    asm_ast.to_asm_string(&mut asm_str, true)?;
    Ok(asm_str)
}

fn parse_asm_to_vec(asm: &str) -> Vec<Vec<String>> {
    asm.lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| {
            // Split by whitespace first to get the opcode/directive/label
            let parts: Vec<&str> = line.splitn(2, char::is_whitespace).collect();

            let mut components = Vec::new();
            // 1. Add the first part (Opcode, Label, or Directive)
            components.push(parts[0].to_string());

            // 2. Add Operands (if any)
            if parts.len() > 1 {
                let operands: Vec<String> = parts[1]
                    .split(',')
                    .map(|op| op.trim().to_string())
                    .collect();
                components.extend(operands);
            }

            components
        })
        .collect()
}

fn assert_program_structure(
    lines: &[Vec<String>],
    func_name: &str,
    expect_stack_alloc: bool,
    mut line: usize,
) -> usize {
    assert!(
        lines[line][0] == ".globl" && lines[line][1] == func_name,
        "Missing '.globl {}' directive",
        func_name
    );

    line += 1;
    assert!(
        lines[line][0] == format!("{}:", func_name),
        "Missing function label '{}:'",
        func_name
    );

    let prologue_1 = vec!["pushq".to_string(), "%rbp".to_string()];
    let prologue_2 = vec!["movq".to_string(), "%rsp".to_string(), "%rbp".to_string()];

    line += 1;
    assert_eq!(
        lines[line], prologue_1,
        "Incorrect prologue: expected 'pushq %rbp'"
    );
    assert_eq!(
        lines[line + 1],
        prologue_2,
        "Incorrect prologue: expected 'movq %rsp, %rbp'"
    );

    line += 2;
    if expect_stack_alloc {
        assert!(
            lines[line][0] == "subq" && lines[line][2] == "%rsp",
            "Expected stack allocation with 'subq' instruction"
        );
        line += 1;
    }

    let has_stack_note = lines.iter().any(|line| {
        line[0] == ".section"
            && line
                .get(1)
                .map_or(false, |op| op.contains(".note.GNU-stack"))
    });
    assert!(
        has_stack_note,
        "Missing '.section .note.GNU-stack' directive"
    );
    line
}

fn assert_function_end(lines: &[Vec<String>], mut line: usize) {
    let epilogue_1 = vec!["movq".to_string(), "%rbp".to_string(), "%rsp".to_string()];
    let epilogue_2 = vec!["popq".to_string(), "%rbp".to_string()];
    let ret_instr = vec!["ret".to_string()];

    assert_eq!(
        lines[line], epilogue_1,
        "Incorrect epilogue: expected 'movq %rbp, %rsp'"
    );
    line += 1;
    assert_eq!(
        lines[line], epilogue_2,
        "Incorrect epilogue: expected 'popq %rbp'"
    );
    line += 1;
    assert_eq!(lines[line], ret_instr, "Incorrect epilogue: expected 'ret'");
}

#[test]
fn test_return_0_clean() {
    let src = "int main(void) { return 0; }";
    let asm_str = compile_to_asm_string(src).unwrap();
    let lines = parse_asm_to_vec(&asm_str);

    let mut line = assert_program_structure(&lines, "main", false, 0);
    assert!(lines[line] == vec!["movq".to_string(), "$0".to_string(), "%rax".to_string()]);
    line += 1;
    assert_function_end(&lines, line);
}

#[test]
fn test_unary_negate_clean() {
    let src = "int main(void) { return -5; }";
    let asm_str = compile_to_asm_string(src).unwrap();
    let lines = parse_asm_to_vec(&asm_str);
    println!("lines: {:#?}", lines);

    let mut line = assert_program_structure(&lines, "main", true, 0);

    assert!(lines[line] == vec!["movq".to_string(), "$5".to_string(), "-8(%rbp)".to_string()]);
    line += 1;
    assert!(lines[line] == vec!["negq".to_string(), "-8(%rbp)".to_string()]);
    line += 1;
    assert!(
        lines[line]
            == vec![
                "movq".to_string(),
                "-8(%rbp)".to_string(),
                "%rax".to_string()
            ]
    );
    line += 1;
    assert_function_end(&lines, line);
}
