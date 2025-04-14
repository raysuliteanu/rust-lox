use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, Report, WrapErr};
use rust_lox::token::Lexer;
use rust_lox::{interpret, parser};
use std::path::PathBuf;
use std::process::ExitCode;
use std::{fs, str};

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

    let result = match &lox.commands {
        LoxCommands::Tokenize { filename } => {
            let source = get_source(filename)?;
            let lexer = Lexer::new(filename.display().to_string(), source.as_str());
            lexer.tokenize()
        }
        LoxCommands::Parse { filename } => {
            let source = get_source(filename)?;
            let lexer = Lexer::new(filename.display().to_string(), source.as_str());
            let mut parser = parser::Parser::new(lexer.peekable());
            parser.parse()
        }
        LoxCommands::Evaluate { filename } => {
            let source = get_source(filename)?;
            let interpreter = interpret::Interpreter::new(&source, filename);
            interpreter.evaluate()
        }
    };

    let exit_code = match result {
        Ok(_) => 0u8,
        Err(e) => {
            eprintln!("{e}");
            e.code()
                .unwrap_or(Box::new("1"))
                .to_string()
                .parse::<u8>()
                .into_diagnostic()?
        }
    };

    Ok(ExitCode::from(exit_code))
}

fn get_source(filename: &PathBuf) -> Result<String, Report> {
    let source = fs::read_to_string(filename)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read {}", filename.display()))?;
    Ok(source)
}
