use clap::Parser;
use colored::Colorize;
use minic::lexer_error::LexerError;
use minic::parser_error::ParserError;
use minic::source_file::SourceFile;
use std::fs;
use std::process;

#[derive(Parser)]
#[command(name = "minic", version = "0.1.0")]
#[command(version, about, long_about = None)]
struct Cli {
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
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();
    for file in cli.files {
        let source = fs::read_to_string(&file)?;
        let source_file = SourceFile::new(file.clone(), source);

        let compile_result = minic::compile(&source_file, cli.lex);

        let Ok(_) = compile_result else {
            let compile_err = compile_result.unwrap_err();
            let msg = compile_err.to_string();
            let range = compile_err.downcast_ref::<LexerError>().map(|e| e.range);

            if let Some(lexer_err) = range {
                let line_col_start = source_file.line_col(lexer_err.0.0);
                let line_col_end = source_file.line_col(lexer_err.1.0);
                eprintln!(
                    "{}",
                    format!(
                        "Error in file {} at {}:{} - {}:{} (at `{}`) :: {}",
                        source_file.filename(),
                        line_col_start.0,
                        line_col_start.1,
                        line_col_end.0,
                        line_col_end.1,
                        source_file[lexer_err.0.0..lexer_err.1.0].replace('\n', "\\n"),
                        msg
                    )
                    .red()
                );
                process::exit(-1);
            }

            if let Some(parse_err) = compile_err.downcast_ref::<ParserError>() {
                let line_col_start = source_file.line_col(parse_err.span.0.0);
                let line_col_end = source_file.line_col(parse_err.span.1.0);
                eprintln!(
                    "{}",
                    format!(
                        "Error in file {} at {}:{} - {}:{} (at `{}`) :: {}",
                        source_file.filename(),
                        line_col_start.0,
                        line_col_start.1,
                        line_col_end.0,
                        line_col_end.1,
                        source_file[parse_err.span.0.0..parse_err.span.1.0].replace('\n', "\\n"),
                        msg
                    )
                    .red()
                );
            }
            process::exit(-1);
        };
    }
    Ok(())
}
