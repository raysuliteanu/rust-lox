use crate::token::Lexer;
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::str;
use miette::{IntoDiagnostic, WrapErr};

mod codecrafters;
mod token;

#[derive(Parser)]
struct Lox {
    #[command(subcommand)]
    commands: LoxCommands,
}

#[derive(Subcommand)]
enum LoxCommands {
    Tokenize { filename: PathBuf },
    Parse { filename: PathBuf },
    Evaluate { filename: PathBuf },
}

fn main() -> Result<(), miette::Error> {
    let lox = Lox::parse();

    let _ = match &lox.commands {
        LoxCommands::Tokenize { filename } => {
            let source = std::fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read {}", filename.display()))?;
            
            let mut lexer = Lexer::new(filename.display().to_string(), source.as_str());

            lexer.tokenize()
        }
        LoxCommands::Parse { filename } => todo!(),
        LoxCommands::Evaluate { filename } => todo!(),
    }
    .map_err(|_| std::process::exit(65))
    .unwrap();

    Ok(())
}
