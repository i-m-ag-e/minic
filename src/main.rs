use clap::Parser;
use colored::Colorize;
use minic::Cli;
use minic::compile;
use std::process;

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();
    if let Err(e) = compile(&cli) {
        eprintln!("{}", e.to_string().red());
        process::exit(1);
    }
    Ok(())
}
