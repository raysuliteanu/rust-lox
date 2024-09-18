use crate::error::{InterpreterResult};
use crate::parser::Ast;
use crate::token::LexToken;
use clap::{Parser, Subcommand};
use lex::Scanner;
use std::error::Error;
use std::path::PathBuf;
use std::str;
use crate::interpreter::ExprResult;

mod codecrafters;
mod error;
mod interpreter;
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
    Evaluate { filename: PathBuf },
}

fn main() -> InterpreterResult<()> {
    let lox = Lox::parse();

    match &lox.commands {
        LoxCommands::Tokenize { filename } => tokenize(filename, true).map(|_| {
            Ok(()) 
        }),
        LoxCommands::Parse { filename } => parse(tokenize(filename, false)?).map(|ast| {
            println!("{ast}");
            Ok(())
        }),
        LoxCommands::Evaluate { filename } => {
            evaluate(parse(tokenize(filename, false)?)?).map(|expr| {
                println!("{expr}");
                Ok(()) 
            })
        }
    }
    .map_err(|_: Box<dyn Error>| std::process::exit(65))
    .unwrap()
}

fn tokenize(filename: &PathBuf, tokenization_only: bool) -> InterpreterResult<Vec<LexToken>> {
    let mut scanner = Scanner::new(filename)?;
    scanner.tokenize(tokenization_only)
}

fn parse(tokens: Vec<LexToken>) -> InterpreterResult<Ast> {
    let parser = parser::PrattParser::new(tokens);
    parser.parse()
}

fn evaluate(ast: Ast) -> InterpreterResult<ExprResult> {
    let interpreter = interpreter::Interpreter::new(ast);
    interpreter.interpret()
}
