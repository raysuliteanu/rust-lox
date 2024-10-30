use crate::token::Lexer;
use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, Report, WrapErr};
use std::path::PathBuf;
use std::process::ExitCode;
use std::{fs, str};

mod interpret;
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
    Evaluate { filename: PathBuf },
}

fn main() -> Result<ExitCode, miette::Error> {
    env_logger::init();

    let lox = Lox::parse();

    let exit_code = match &lox.commands {
        LoxCommands::Tokenize { filename } => {
            let source = get_source(filename)?;

            let lexer = Lexer::new(filename.display().to_string(), source.as_str());

            tokenize(lexer)?
        }
        LoxCommands::Parse { filename } => {
            let source = get_source(filename)?;

            let lexer = Lexer::new(filename.display().to_string(), source.as_str());
            let mut parser = parser::Parser::new(lexer.peekable());
            parse(&mut parser)?
        }
        LoxCommands::Evaluate { filename } => {
            let source = get_source(filename)?;

            let lexer = Lexer::new(filename.display().to_string(), source.as_str());
            let mut parser = parser::Parser::new(lexer.peekable());

            let ast = parser.parse()?;
            let interpreter = interpret::Interpreter::new(ast);
            let value = interpreter.evaluate();
            match value {
                Ok(t) => {
                    println!("{t}");
                    0u8
                }
                Err(e) => {
                    eprintln!("{e}");
                    65u8
                }
            }
        }
    };

    Ok(ExitCode::from(exit_code))
}

pub fn tokenize(lexer: Lexer) -> Result<u8, miette::Error> {
    let mut exit_code = 0u8;
    for next in lexer {
        match next {
            Ok(t) => {
                println!("{t}");
            }
            Err(e) => {
                exit_code = 65;
                eprintln!("{e}");
            }
        }
    }

    println!("EOF  null");

    Ok(exit_code)
}

pub fn parse(parser: &mut parser::Parser) -> Result<u8, miette::Error> {
    let mut exit_code = 0u8;
    let ast = parser.parse();
    match ast {
        Ok(t) => {
            println!("{t}");
        }
        Err(e) => {
            exit_code = 65;
            eprintln!("{e}");
        }
    }

    Ok(exit_code)
}

fn get_source(filename: &PathBuf) -> Result<String, Report> {
    let source = fs::read_to_string(filename)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read {}", filename.display()))?;
    Ok(source)
}
