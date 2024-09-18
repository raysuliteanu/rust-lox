use std::error::Error;

pub type InterpreterResult<T> = Result<T, Box<dyn Error>>;

#[derive(Debug, thiserror::Error)]
pub enum ParserError<'a> {
    #[error("[line {line}] Error at {token}: Expect expression.")]
    InvalidExpression { line: usize, token: &'a str },
    #[error("[line {line}] Unexpected EOF.")]
    UnexpectedEof { line: usize },
}


#[derive(Debug, thiserror::Error)]
pub enum LexerError {
    #[error("[line {line}] Error: Unexpected character: {token}")]
    InvalidToken { line: usize, token: char },
    #[error("[line {line}] Error: Unterminated string.")]
    UnterminatedString { line: usize },
}
