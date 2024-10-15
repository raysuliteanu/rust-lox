use std::fmt::Display;
use std::iter::Peekable;

use miette::Diagnostic;
use thiserror::Error;

use crate::token;
use crate::token::Lexer;
use crate::token::LiteralKind;
use crate::token::Token;

pub struct Parser<'pa> {
    lexer: Peekable<Lexer<'pa>>,
}

type ParserResult = Result<Node, miette::Error>;

impl<'pa> Parser<'pa> {
    pub(crate) fn new(lexer: Peekable<Lexer<'pa>>) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<Ast, miette::Error> {
        Ok(Ast {
            tree: self.expression()?,
        })
    }

    fn matches(&mut self, tokens: &[Token]) -> bool {
        if let Some(next) = self.lexer.peek() {
            match next {
                Ok(t) => tokens.iter().any(|m| m == t),
                Err(_) => false,
            }
        } else {
            false
        }
    }

    fn expression(&mut self) -> ParserResult {
        self.equality()
    }

    fn equality(&mut self) -> ParserResult {
        eprintln!("equality()");

        let mut left = self.comparison()?;

        while self.matches(&[
            Token::Literal(LiteralKind::BangEq),
            Token::Literal(LiteralKind::EqEq),
        ]) {
            eprintln!("matched '!=' | '=='");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.comparison()?;
            left = Node::Expr(Box::new(Expr::Binary(
                Box::new(left),
                Node::Terminal(op),
                Box::new(right),
            )));
        }

        Ok(left)
    }

    fn comparison(&mut self) -> ParserResult {
        eprintln!("comparison()");

        let mut left = self.term()?;

        while self.matches(&[
            Token::Literal(LiteralKind::Greater),
            Token::Literal(LiteralKind::GreaterEq),
            Token::Literal(LiteralKind::Less),
            Token::Literal(LiteralKind::LessEq),
        ]) {
            eprintln!("matched comparison");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.term()?;

            left = Node::Expr(Box::new(Expr::Binary(
                Box::new(left),
                Node::Terminal(op),
                Box::new(right),
            )));
        }

        Ok(left)
    }

    fn term(&mut self) -> ParserResult {
        eprintln!("term()");

        let mut left = self.factor()?;

        while self.matches(&[
            Token::Literal(LiteralKind::Plus),
            Token::Literal(LiteralKind::Minus),
        ]) {
            eprintln!("matched '+' | '-'");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.factor()?;

            left = Node::Expr(Box::new(Expr::Binary(
                Box::new(left),
                Node::Terminal(op),
                Box::new(right),
            )));
        }

        Ok(left)
    }

    fn factor(&mut self) -> ParserResult {
        eprintln!("factor()");
        let mut left = self.unary()?;
        while self.matches(&[
            Token::Literal(LiteralKind::Star),
            Token::Literal(LiteralKind::Slash),
        ]) {
            eprintln!("matched '*' | '/'");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.unary()?;

            left = Node::Expr(Box::new(Expr::Binary(
                Box::new(left),
                Node::Terminal(op),
                Box::new(right),
            )));
        }

        Ok(left)
    }

    fn unary(&mut self) -> ParserResult {
        eprintln!("unary()");
        if self.matches(&[
            Token::Literal(LiteralKind::Minus),
            Token::Literal(LiteralKind::Bang),
        ]) {
            eprintln!("matched unary '!' | '-'");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.unary()?;

            Ok(Node::Expr(Box::new(Expr::Unary(
                Node::Terminal(op),
                Box::new(right),
            ))))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ParserResult {
        eprintln!("primary()");

        if self.matches(&[Token::Keyword(token::KeywordKind::True)]) {
            return Ok(Node::Terminal(Token::Keyword(token::KeywordKind::True)));
        }
        if self.matches(&[Token::Keyword(token::KeywordKind::False)]) {
            return Ok(Node::Terminal(Token::Keyword(token::KeywordKind::False)));
        }
        if self.matches(&[Token::Keyword(token::KeywordKind::Nil)]) {
            return Ok(Node::Terminal(Token::Keyword(token::KeywordKind::Nil)));
        }
        if self.matches(&[Token::Literal(token::LiteralKind::LeftParen)]) {
            // eat left paren
            self.lexer.next();

            let expr = self.expression()?;

            // eat right paren
            assert!(self
                .lexer
                .next()
                .is_some_and(|r| r.is_ok_and(|t| t == Token::Literal(LiteralKind::RightParen))));

            return Ok(Node::Expr(Box::new(Expr::Group(Box::new(expr)))));
        }

        eprintln!("checking for number, string, identifier");
        if let Some(token) = self.lexer.next() {
            match token {
                Ok(t) => match t {
                    Token::Number { .. } | Token::String { .. } | Token::Identifier { .. } => {
                        eprintln!("matched '{t}'");
                        Ok(Node::Terminal(t))
                    }
                    _ => {
                        eprintln!("unexpected token '{t}'");
                        Err(Eof.into())
                    }
                },
                Err(e) => Err(e),
            }
        } else {
            eprintln!("lexer.next() returned None");
            Err(Eof.into())
        }
    }
}

pub struct Ast {
    tree: Node,
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.tree)
    }
}

enum Node {
    Terminal(Token),
    Expr(Box<Expr>),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Terminal(t) => write!(f, "{t}"),
            Node::Expr(e) => write!(f, "{e}"),
        }
    }
}

enum Expr {
    Unary(Node, Box<Node>),
    Binary(Box<Node>, Node, Box<Node>),
    Group(Box<Node>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unary(t, e) => write!(f, "{t} {e}"),
            Expr::Binary(l, t, r) => write!(f, "{l} {t} {r}"),
            Expr::Group(e) => write!(f, "(group {})", e),
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("Unexpected EOF")]
pub struct Eof;

enum _Stmt {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_ast() {
        let ast = Ast {
            tree: Node::Expr(Box::new(Expr::Binary(
                Box::new(Node::Terminal(Token::Number {
                    raw: "1.23".to_string(),
                    value: 1.23,
                })),
                Node::Terminal(Token::Literal(LiteralKind::Star)),
                Box::new(Node::Terminal(Token::Number {
                    raw: "1.23".to_string(),
                    value: 1.23,
                })),
            ))),
        };

        let fmt = format!("{ast}");
        assert_eq!(fmt, "(1.23 * 1.23)");
    }
}
