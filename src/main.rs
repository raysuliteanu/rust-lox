use crate::token::Token;
use clap::{Parser, Subcommand};
use lex::Scanner;
use std::path::PathBuf;
use std::str;
use anyhow::Error;

mod lex;
mod parser;
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
}

fn main() {
    let lox = Lox::parse();

    match &lox.commands {
        LoxCommands::Tokenize { filename } => {
            let res = tokenize(filename, true);
            
            if res.is_err() {
                std::process::exit(65);
            }
        }
        LoxCommands::Parse { filename } => {
            // first need to tokenize
            let _ = tokenize(filename, false).and_then(|tokens| {
                let parser = parser::PrattParser::new(tokens);
                let ast = parser.parse()?;
                println!("{ast}");
                Ok(())
            });
        }
    }
}

fn tokenize(filename: &PathBuf, tokenization_only: bool) -> anyhow::Result<Vec<Token>> {
    let mut scanner = Scanner::new(filename, tokenization_only)?;
    scanner.tokenize()?;
    if scanner.has_tokenization_err() {
        return Err(Error::msg("Tokenization failed"));
    }

    Ok(scanner.tokens)
}
