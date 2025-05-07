use std::fmt::Display;
use std::fmt::Write as _;
use std::iter::Peekable;

use log::trace;
use miette::Diagnostic;
use strum::EnumMessage;
use thiserror::Error;

use crate::token;
use crate::token::KeywordKind;
use crate::token::LiteralKind;
use crate::token::Token;

use crate::literal_token;

pub struct Parser<T: Iterator> {
    lexer: Peekable<T>,
}

pub type ParserResult<T> = Result<T, miette::Error>;

macro_rules! binary_node {
    ($l:ident,$m:ident,$r:ident) => {
        Node::Expr(Box::new(Expr::Binary(
            Box::new($l),
            Node::Terminal($m),
            Box::new($r),
        )))
    };
}

impl<T> Parser<T>
where
    T: Iterator<Item = Result<Token, miette::Error>>,
{
    pub fn new(lexer: Peekable<T>) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<u8, miette::Error> {
        let ast = self.ast()?;
        println!("{ast}");
        Ok(0)
    }

    pub fn ast(&mut self) -> Result<Ast, miette::Error> {
        Ok(Ast {
            tree: self.program()?,
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

    fn program(&mut self) -> ParserResult<Vec<Node>> {
        trace!("program()");
        let mut program = vec![];
        while let Some(result) = self.statement() {
            match result {
                Ok(n) => program.push(n),
                Err(e) => return Err(e),
            }
        }

        Ok(program)
    }

    // statement => print_statement | expression_statement ";"
    fn statement(&mut self) -> Option<ParserResult<Node>> {
        trace!("statement()");
        if let Some(Ok(t)) = self.lexer.peek() {
            let node = match t {
                Token::Keyword(keyword_kind) => match keyword_kind {
                    KeywordKind::Print => self.print_statement(),
                    _ => unimplemented!(),
                },
                _ => self.expression_statement(),
            };

            let next = self.lexer.next();
            if let Some(Ok(next_token)) = next {
                if next_token == literal_token!(';') {
                    Some(node)
                } else {
                    Some(Err(MissingToken {
                        expected: literal_token!(';'),
                        actual: next_token,
                    }
                    .into()))
                }
            } else {
                Some(Err(MissingToken {
                    expected: literal_token!(';'),
                    actual: Token::Eof,
                }
                .into()))
            }
        } else {
            trace!("EOF");
            None
        }
    }

    // print_statement => "print" expression
    fn print_statement(&mut self) -> ParserResult<Node> {
        trace!("print_statement()");

        let print_token = self.lexer.next();
        assert_eq!(
            Token::Keyword(KeywordKind::Print),
            print_token.unwrap().unwrap()
        );

        let exp = self.expression()?;
        // build print statement AST node
        Ok(Node::Stmt {
            ty: KeywordKind::Print,
            exp: Box::new(exp),
        })
    }

    fn expression_statement(&mut self) -> ParserResult<Node> {
        trace!("expression_statement()");
        self.expression()
    }

    pub(crate) fn expression(&mut self) -> ParserResult<Node> {
        trace!("expression()");
        self.equality()
    }

    fn equality(&mut self) -> ParserResult<Node> {
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

    fn comparison(&mut self) -> ParserResult<Node> {
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

    fn term(&mut self) -> ParserResult<Node> {
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

    fn factor(&mut self) -> ParserResult<Node> {
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

    fn unary(&mut self) -> ParserResult<Node> {
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

    fn primary(&mut self) -> ParserResult<Node> {
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

            match self.lexer.next() {
                Some(r) => match r {
                    Ok(t) => {
                        if t == Token::Literal(LiteralKind::RightParen) {
                            let group = Node::Expr(Box::new(Expr::Group(Box::new(expr))));
                            trace!("matched ')'; consuming it and returning group {group}");
                            return Ok(group);
                        } else {
                            return Err(MissingToken {
                                expected: Token::Literal(LiteralKind::RightParen),
                                actual: t,
                            }
                            .into());
                        }
                    }
                    Err(e) => {
                        return Err(e);
                    }
                },
                None => {
                    return Err(UnexpectedEof.into());
                }
            }
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
                        Err(UnexpectedToken { token: t }.into())
                    }
                },
                Err(e) => Err(e),
            }
        } else {
            trace!("lexer.next() returned None");
            Err(UnexpectedEof.into())
        }
    }
}

pub struct Ast {
    pub tree: Vec<Node>,
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        for n in &self.tree {
            write!(&mut buf, "{n}")?
        }
        write!(f, "{buf}")
    }
}

#[derive(Debug)]
pub enum Node {
    Terminal(Token),
    Expr(Box<Expr>),
    Stmt { ty: KeywordKind, exp: Box<Node> },
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Terminal(t) => match t {
                Token::Keyword(k) => write!(
                    f,
                    "{}",
                    <&KeywordKind as Into<&'static str>>::into(k).to_lowercase()
                ),
                Token::Literal(l) => write!(f, "{}", l.get_message().unwrap()),
                Token::Number { value, .. } => {
                    if *value == value.trunc() {
                        // tests require that integers are printed as N.0
                        write!(f, "{value}.0")
                    } else {
                        write!(f, "{value}")
                    }
                }
                Token::Identifier { value } | Token::String { value } => write!(f, "{value}"),
                Token::Eof => write!(f, ""),
            },
            Node::Expr(e) => write!(f, "{e}"),
            Node::Stmt { ty, exp } => write!(f, "{} {}", ty, exp),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Unary(Node, Box<Node>),
    Binary(Box<Node>, Node, Box<Node>),
    Group(Box<Node>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unary(t, e) => write!(f, "({t} {e})"),
            Expr::Binary(l, t, r) => write!(f, "({t} {l} {r})"),
            Expr::Group(e) => write!(f, "(group {})", e),
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("Unexpected EOF")]
#[diagnostic(code("65"))]
pub struct UnexpectedEof;

#[derive(Error, Debug, Diagnostic)]
#[error("missing token {expected} got {actual}")]
#[diagnostic(code("65"))]
pub struct MissingToken {
    expected: Token,
    actual: Token,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Unexpected token {token}")]
#[diagnostic(code("65"))]
pub struct UnexpectedToken {
    token: Token,
}

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
        assert_eq!(fmt, "(* 1.23 1.23)");
    }

    #[test]
    fn print_ast() {
        let ast = Ast {
            tree: vec![Node::Expr(Box::new(Expr::Binary(
                Box::new(Node::Terminal(Token::Number {
                    raw: "1.23".to_string(),
                    value: 1.23,
                })),
                Node::Terminal(Token::Literal(LiteralKind::Star)),
                Box::new(Node::Terminal(Token::Number {
                    raw: "1.23".to_string(),
                    value: 1.23,
                })),
            )))],
        };

        let fmt = format!("{ast}");
        assert_eq!(fmt, "(* 1.23 1.23)");
    }
}
