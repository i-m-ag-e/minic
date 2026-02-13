mod asm;
mod ast;
mod debug_info;
mod lexer;
mod parser;
mod pretty_print;
pub mod source_file;
mod symbol;
mod tacky;
mod with_token;

use std::{
    fs,
    path::Path,
    process::{Command, Stdio},
};

use anyhow::{self, bail};
use clap::{self, CommandFactory};
use lexer::Lexer;
use source_file::SourceFile;
use symbol::SymbolTable;

pub use lexer::lexer_error;
pub use parser::parser_error;

use crate::{
    asm::tacky_to_asm,
    ast::ASTRefVisitor,
    lexer::{LexerResult, token::TokenType},
    parser::Parser,
    source_file::SourcePosition,
};

#[derive(clap::Parser)]
#[command(name = "minic", version = "0.1.0")]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Input source files
    files: Vec<String>,

    /// specify path to output file
    #[arg(long, short)]
    output: Option<String>,

    /// generate object file
    #[arg(short = 'c')]
    object_file: bool,

    /// only run the lexer
    #[arg(long)]
    lex: bool,

    /// only run the lexer and parser
    #[arg(long)]
    parse: bool,

    /// only run till the codegen phase
    #[arg(long)]
    codegen: bool,

    /// only generate tacky IR
    #[arg(long)]
    tacky: bool,

    /// only output assembly files
    #[arg(short = 'S', long)]
    only_assembly: bool,

    /// don't generate comments in assembly output
    #[arg(long)]
    no_comments: bool,

    /// don't output assembly to stdout
    #[arg(short = 'O', long)]
    no_stdout: bool,

    /// don't keep assembly files after compilation;
    /// ignored if --only-assembly is set
    #[arg(short = 'K', long)]
    no_keep_asm: bool,

    /// debug mode; prints additional debug information during compilation
    #[arg(long)]
    debug: bool,
}

pub fn assemble(
    source_file: &mut SourceFile,
    stop_at_lex: bool,
    stop_at_parse: bool,
    stop_at_tacky: bool,
    stop_at_codegen: bool,
    no_stdout: bool,
    no_comments: bool,
    debug: bool,
) -> anyhow::Result<String> {
    let mut interner = SymbolTable::new();
    let lexer = Lexer::new(source_file, &mut interner);
    let tokens: Vec<_> = lexer.collect::<LexerResult<Vec<_>>>()?;
    for token in &tokens {
        let lexeme = &source_file[token.begin.0..token.end.0];
        let line_col_start = source_file.line_col(token.begin.0);
        let line_col_end = source_file.line_col(token.end.0);

        if debug {
            print!(
                "{:#?} <{:?} :: ({}:{} - {}:{}>)",
                token, lexeme, line_col_start.0, line_col_start.1, line_col_end.0, line_col_end.1
            );
        }

        if let TokenType::Identifier(sym) = &token.token_type {
            let name = interner.resolve(*sym).unwrap();
            if debug {
                println!(" (ident: {:?})", name);
            }
        } else if debug {
            println!("");
        }
    }

    let prog = if !stop_at_lex {
        let mut used_tokens = vec![false; tokens.len()];
        let mut parser = Parser::new(&tokens, &mut used_tokens);

        let prog = parser.parse()?;
        source_file.set_tokens(Parser::filter_saved_tokens(tokens, &mut used_tokens));

        let mut pretty_printer = pretty_print::PrettyPrinter::new();
        if debug {
            pretty_printer.visit_program(&prog);
        }
        // println!("{:#?}", prog);
        prog
    } else {
        return Ok(String::new());
    };

    let tacky_prog = if !stop_at_parse {
        let mut tacky_gen = tacky::tacky_gen::TackyGen::new(source_file);
        let tacky_prog = tacky_gen.visit_program(&prog);
        if debug {
            println!("{:#?}", tacky_prog);
        }
        tacky_prog
    } else {
        return Ok(String::new());
    };

    let asm_program = if !stop_at_tacky {
        let asm_program = tacky_to_asm(&tacky_prog);
        if debug {
            println!("{:#?}", asm_program);
        }
        asm_program
    } else {
        return Ok(String::new());
    };

    if !stop_at_codegen {
        let mut asm_code = String::new();
        asm_program.to_asm_string(&mut asm_code, no_comments)?;
        if !no_stdout {
            println!("{}", asm_code);
        }
        return Ok(asm_code);
    }
    Ok(String::new())
}

fn make_error(
    source_file: &SourceFile,
    msg: &str,
    span: (SourcePosition, SourcePosition),
) -> anyhow::Error {
    let line_col_start = source_file.line_col(span.0.0);
    let line_col_end = source_file.line_col(span.1.0);
    anyhow::anyhow!(
        "{}",
        format!(
            "Error in file {} at {}:{} - {}:{} (at `{}`) :: {}",
            source_file.filename(),
            line_col_start.0,
            line_col_start.1,
            line_col_end.0,
            line_col_end.1,
            source_file[span.0.0..span.1.0].replace('\n', "\\n"),
            msg
        )
    )
}

fn handle_compile_error(compile_err: anyhow::Error, source_file: &SourceFile) -> anyhow::Error {
    let msg = compile_err.to_string();
    let range = compile_err
        .downcast_ref::<lexer_error::LexerError>()
        .map(|e| e.range);

    if let Some(lexer_err) = range {
        make_error(source_file, &msg, lexer_err)
    } else if let Some(parse_err) = compile_err.downcast_ref::<parser_error::ParserError>() {
        make_error(source_file, &msg, parse_err.span)
    } else {
        compile_err
    }
}

fn preprocess_source_file(file: &String, out_file: &String) -> anyhow::Result<SourceFile> {
    let gcc = Command::new("gcc")
        .arg("-E")
        .arg(file)
        .stdout(Stdio::piped())
        .spawn()?;
    let output = gcc.wait_with_output()?;
    if !output.status.success() {
        bail!("Preprocessing failed for file {}", file);
    }

    let output = String::from_utf8(output.stdout)?;
    let output = output
        .lines()
        .skip_while(|line| line.starts_with('#'))
        .collect::<Vec<_>>()
        .join("\n");
    Ok(SourceFile::new(out_file.clone(), output))
}

pub fn compile(cli: &Cli) -> anyhow::Result<()> {
    // let asm = assemble(source_file, cli.lex, cli.parse, cli.codegen)?;

    if cli.files.is_empty() {
        Cli::command().print_help()?;
        return Ok(());
    }

    if cli.output.is_some() && cli.object_file && cli.files.len() > 1 {
        bail!("Cannot specify output file when compiling multiple source files to object files",);
    }

    let asm_files = cli
        .files
        .iter()
        .map(|file| {
            Ok(Path::new(file)
                .with_extension("s")
                .to_str()
                .ok_or(anyhow::anyhow!("invalid characters in file path"))?
                .to_owned())
        })
        .collect::<anyhow::Result<Vec<String>>>()?;

    let pp_files = cli
        .files
        .iter()
        .map(|file| {
            Ok(Path::new(file)
                .with_extension("i")
                .to_str()
                .ok_or(anyhow::anyhow!("invalid characters in file path"))?
                .to_owned())
        })
        .collect::<anyhow::Result<Vec<String>>>()?;

    for ((asm_file, pp_file), source_file) in asm_files.iter().zip(&pp_files).zip(&cli.files) {
        let mut source = preprocess_source_file(source_file, pp_file)?;
        if cli.debug {
            println!("Source: {:?}", source);
        }
        let asm = assemble(
            &mut source,
            cli.lex,
            cli.parse,
            cli.tacky,
            cli.codegen,
            cli.no_stdout,
            cli.no_comments,
            cli.debug,
        )
        .map_err(|e| handle_compile_error(e, &source))?;

        if cli.lex || cli.parse || cli.tacky || cli.codegen {
            continue;
        }

        fs::write(asm_file, asm)
            .map_err(|e| anyhow::anyhow!("Failed to write assembly file `{}`: {}", asm_file, e))?;
    }

    if cli.lex || cli.parse || cli.tacky || cli.codegen || cli.only_assembly {
        return Ok(());
    }

    let mut options = vec![];
    if cli.object_file {
        options.push("-c".to_owned());
    }

    if let Some(ref output) = cli.output {
        options.push("-o".to_owned());
        options.push(output.to_owned());
    } else if asm_files.len() == 1 {
        let output_file = Path::new(&asm_files[0])
            .with_extension("")
            .to_str()
            .ok_or(anyhow::anyhow!("invalid characters in file path"))?
            .to_owned();
        options.push("-o".to_owned());
        options.push(output_file);
    }

    let status = Command::new("gcc")
        .args(&asm_files)
        .args(&options)
        .status()
        .map_err(|e| anyhow::anyhow!("Failed to invoke gcc: {}", e))?;

    if !status.success() {
        bail!("gcc failed with exit code: {}", status);
    }

    if cli.no_keep_asm {
        for asm_file in &asm_files {
            fs::remove_file(asm_file).map_err(|e| {
                anyhow::anyhow!("Failed to remove assembly file `{}`: {}", asm_file, e)
            })?;
        }
    }

    Ok(())
}
