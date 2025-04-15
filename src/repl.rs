use std::io::{BufRead, Write, stdout};

use crate::interpret;

pub struct Repl<'r> {
    interpreter: interpret::Interpreter<'r>,
}

impl<'r> Repl<'r> {
    pub fn new() -> Self {
        Self {
            interpreter: interpret::Interpreter::default(),
        }
    }

    pub fn run(&'r mut self) -> Result<(), miette::Error> {
        let mut stdin = std::io::stdin().lock();
        let mut expr = String::new();
        loop {
            print!("lox: ");
            let _ = stdout().flush();
            let _line = stdin
                .read_line(&mut expr)
                .map_err(miette::Error::from_err)?;

            let source = expr.trim().to_owned();

            // for now, since we only support expressions, just evaluate each line
            // no need for some kind of "run" repl command
            if source == "exit" || source == "quit" || source == "q" {
                break;
            }
            let _ = self.interpreter.interpret(source).map_err(|e| {
                eprintln!("{e}");
            });
            expr.clear();
        }

        Ok(())
    }
}

impl Default for Repl<'_> {
    fn default() -> Self {
        Self::new()
    }
}
