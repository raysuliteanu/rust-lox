use crate::token::Lexer;

pub struct Parser<'pa> {
    lexer: Lexer<'pa>,
}

impl<'pa> Parser<'pa> {
    pub(crate) fn new(p0: Lexer) -> Self {
        todo!()
    }
}

impl Iterator for Parser<'_> {
    type Item = Option<Result<Token, miette::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

pub struct Token {}