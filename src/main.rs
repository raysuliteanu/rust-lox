use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::str;

mod lex;
mod token;

#[derive(Parser)]
struct Lox {
    #[command(subcommand)]
    commands: LoxCommands,
}

#[derive(Subcommand)]
enum LoxCommands {
    Tokenize { filename: PathBuf },
}

fn main() {
    let lox = Lox::parse();

    match &lox.commands {
        LoxCommands::Tokenize { filename } => {
            if let Ok(mut scanner) = lex::Scanner::new(filename) {
                scanner
                    .tokenize()
                    .unwrap_or_else(|_| std::process::exit(65));
            }
        }
    }
}
