use std::fmt::Display;
use std::iter::Peekable;

use log::trace;
use miette::Diagnostic;
use strum::EnumMessage;
use thiserror::Error;

use crate::token;
use crate::token::KeywordKind;
use crate::token::Lexer;
use crate::token::LiteralKind;
use crate::token::Token;

pub struct Parser<'pa> {
    lexer: Peekable<Lexer<'pa>>,
}

type ParserResult = Result<Node, miette::Error>;

macro_rules! binary_node {
    ($l:ident,$m:ident,$r:ident) => {
        Node::Expr(Box::new(Expr::Binary(
            Box::new($l),
            Node::Terminal($m),
            Box::new($r),
        )))
    };
}

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
        trace!("equality()");

        let mut left = self.comparison()?;

        while self.matches(&[
            Token::Literal(LiteralKind::BangEq),
            Token::Literal(LiteralKind::EqEq),
        ]) {
            trace!("matched '!=' | '=='");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.comparison()?;
            left = binary_node!(left, op, right);
        }

        Ok(left)
    }

    fn comparison(&mut self) -> ParserResult {
        trace!("comparison()");

        let mut left = self.term()?;

        while self.matches(&[
            Token::Literal(LiteralKind::Greater),
            Token::Literal(LiteralKind::GreaterEq),
            Token::Literal(LiteralKind::Less),
            Token::Literal(LiteralKind::LessEq),
        ]) {
            trace!("matched comparison");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.term()?;

            left = binary_node!(left, op, right);
        }

        Ok(left)
    }

    fn term(&mut self) -> ParserResult {
        trace!("term()");

        let mut left = self.factor()?;

        while self.matches(&[
            Token::Literal(LiteralKind::Plus),
            Token::Literal(LiteralKind::Minus),
        ]) {
            trace!("matched '+' | '-'");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.factor()?;

            left = binary_node!(left, op, right);
        }

        Ok(left)
    }

    fn factor(&mut self) -> ParserResult {
        trace!("factor()");
        let mut left = self.unary()?;
        while self.matches(&[
            Token::Literal(LiteralKind::Star),
            Token::Literal(LiteralKind::Slash),
        ]) {
            trace!("matched '*' | '/'");
            let op = self.lexer.next().unwrap().unwrap();
            let right = self.unary()?;

            left = binary_node!(left, op, right);
        }

        Ok(left)
    }

    fn unary(&mut self) -> ParserResult {
        trace!("unary()");
        if self.matches(&[
            Token::Literal(LiteralKind::Minus),
            Token::Literal(LiteralKind::Bang),
        ]) {
            trace!("matched unary '!' | '-'");
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
        trace!("primary()");

        if self.matches(&[Token::Keyword(token::KeywordKind::True)]) {
            self.lexer.next();
            return Ok(Node::Terminal(Token::Keyword(token::KeywordKind::True)));
        }

        if self.matches(&[Token::Keyword(token::KeywordKind::False)]) {
            self.lexer.next();
            return Ok(Node::Terminal(Token::Keyword(token::KeywordKind::False)));
        }

        if self.matches(&[Token::Keyword(token::KeywordKind::Nil)]) {
            self.lexer.next();
            return Ok(Node::Terminal(Token::Keyword(token::KeywordKind::Nil)));
        }

        if self.matches(&[Token::Literal(token::LiteralKind::LeftParen)]) {
            trace!("matched '('; consuming it");
            // eat left paren
            self.lexer.next();

            let expr = self.expression()?;

            trace!("expecting ')' and consuming it");

            // eat right paren
            assert!(self
                .lexer
                .next()
                .is_some_and(|r| r.is_ok_and(|t| t == Token::Literal(LiteralKind::RightParen))));

            return Ok(Node::Expr(Box::new(Expr::Group(Box::new(expr)))));
        }

        trace!("checking for number, string, identifier");
        if let Some(token) = self.lexer.next() {
            match token {
                Ok(t) => match t {
                    Token::Number { .. } | Token::String { .. } | Token::Identifier { .. } => {
                        trace!("matched '{t}'");
                        Ok(Node::Terminal(t))
                    }
                    _ => {
                        trace!("unexpected token '{t}'");
                        Err(Eof.into())
                    }
                },
                Err(e) => Err(e),
            }
        } else {
            trace!("lexer.next() returned None");
            Err(Eof.into())
        }
    }
}

pub struct Ast {
    tree: Node,
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.tree)
    }
}

enum Node {
    Terminal(Token),
    Expr(Box<Expr>),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            //
            Node::Terminal(t) => match t {
                Token::Keyword(k) => write!(
                    f,
                    "{}",
                    <&KeywordKind as Into<&'static str>>::into(k).to_lowercase()
                ),
                Token::Literal(l) => write!(f, "{}", l.get_message().unwrap()),
                Token::Number { value, .. } => write!(f, "{value}"),
                Token::Identifier { value } | Token::String { value } => write!(f, "{value}"),
            },
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
    fn binary_node_macro() {
        let l = Node::Terminal(Token::Number {
            raw: "1.23".to_string(),
            value: 1.23,
        });
        let m = Token::Literal(LiteralKind::Star);
        let r = Node::Terminal(Token::Number {
            raw: "1.23".to_string(),
            value: 1.23,
        });
        let node = binary_node!(l, m, r);
        let fmt = format!("{node}");
        assert_eq!(fmt, "1.23 * 1.23");
    }

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
