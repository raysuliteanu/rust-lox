use crate::codecrafters;
use crate::token::{LexToken, LiteralToken};
use std::fmt::{self, Display};

pub struct PrattParser {
    tokens: Vec<LexToken>,
    current: usize,
}

impl PrattParser {
    pub fn new(tokens: Vec<LexToken>) -> Self {
        PrattParser { tokens, current: 0 }
    }

    pub fn parse(mut self) -> anyhow::Result<Ast> {
        self.parse_expression(0)
    }

    fn parse_expression(&mut self, min_bp: u8) -> anyhow::Result<Ast> {
        let mut lhs = match self.next_token() {
            Some(value) => match value {
                Ast::Atom(AstToken::Expr(ExprType::Group('(', _))) => {
                    let group = self.parse_expression(0)?;

                    // check for closing ')'
                    match self.next_token() {
                        None => return Err(ParserError::UnexpectedEof { line: 0 }.into()),
                        Some(t) => match t {
                            Ast::Atom(AstToken::Expr(ExprType::Group(')', _))) => {}
                            _ => {
                                return Err(ParserError::InvalidExpression {
                                    line: 0,
                                    token: ")",
                                }
                                .into())
                            }
                        },
                    }

                    Ast::Cons(Box::from(value), vec![group])
                }
                Ast::Atom(AstToken::Expr(_)) => {
                    if let Some((_, r_bp)) = self.prefix_binding_power(&value) {
                        let rhs = self.parse_expression(r_bp)?;
                        Ast::Cons(Box::from(value), vec![rhs])
                    } else {
                        value
                    }
                }
                Ast::Atom(AstToken::String(_)) | Ast::Atom(AstToken::Number(_)) => value,
                _ => Ast::Atom(AstToken::Eof),
            },
            None => return Err(ParserError::UnexpectedEof { line: 0 }.into()),
        };

        #[allow(clippy::while_let_loop)]
        loop {
            let peek_token = match self.peek_token() {
                Some(t) => match t {
                    Ast::Atom(AstToken::Expr(_)) => t,
                    Ast::Atom(AstToken::Eof) => break,
                    bt => panic!("bad token: {bt:?}"),
                },
                None => {
                    break;
                }
            };

            if let Some((l_bp, ())) = self.postfix_binding_power(&lhs) {
                if l_bp < min_bp {
                    break;
                }

                let _ = self.next_token();

                lhs = Ast::Cons(Box::from(lhs), vec![peek_token]);

                continue;
            }

            if let Some((l_bp, r_bp)) = self.infix_binding_power(&peek_token) {
                if l_bp < min_bp {
                    break;
                }

                let _ = self.next_token();

                let rhs = self.parse_expression(r_bp)?;

                lhs = Ast::Cons(Box::from(peek_token), vec![lhs, rhs]);

                continue;
            };

            break;
        }

        Ok(lhs)
    }

    fn next_token(&mut self) -> Option<Ast> {
        let token = self.tokens.get(self.current).map(|t| Ast::Atom(t.into()));
        self.current += 1;
        token
    }

    fn peek_token(&mut self) -> Option<Ast> {
        self.tokens.get(self.current).map(|t| Ast::Atom(t.into()))
    }

    fn prefix_binding_power(&mut self, token: &Ast) -> Option<((), u8)> {
        match token {
            Ast::Atom(AstToken::Expr(ExprType::Plus))
            | Ast::Atom(AstToken::Expr(ExprType::Minus))
            | Ast::Atom(AstToken::Expr(ExprType::Bang)) => Some(((), 9)),
            _ => None,
        }
    }

    fn infix_binding_power(&mut self, token: &Ast) -> Option<(u8, u8)> {
        match token {
            Ast::Atom(AstToken::Expr(op)) => match op {
                ExprType::EqEq | ExprType::BangEq => Some((2, 1)),
                ExprType::Less | ExprType::LessEq | ExprType::GreaterEq | ExprType::Greater => {
                    Some((3, 4))
                }
                ExprType::Plus | ExprType::Minus => Some((5, 6)),
                ExprType::Star | ExprType::Slash => Some((7, 8)),
                ExprType::Dot => Some((14, 13)),
                _ => None,
            },
            _ => None,
        }
    }

    // currently no postfix operators
    fn postfix_binding_power(&self, _token: &Ast) -> Option<(u8, ())> {
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Dot,
    Minus,
    Plus,
    Star,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Slash,
    And,
    False,
    For,
    If,
    Or,
    Return,
    True,
    Var,
    While,
    Print,
    Nil,
    Group(char, Box<AstToken>),
}

impl Display for ExprType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::And => write!(f, "and"),
            ExprType::False => write!(f, "false"),
            ExprType::For => write!(f, "for"),
            ExprType::If => write!(f, "if"),
            ExprType::Or => write!(f, "or"),
            ExprType::Print => write!(f, "print"),
            ExprType::Return => write!(f, "return"),
            ExprType::True => write!(f, "true"),
            ExprType::Var => write!(f, "var"),
            ExprType::While => write!(f, "while"),
            ExprType::Nil => write!(f, "nil"),
            ExprType::Dot => write!(f, "."),
            ExprType::Minus => write!(f, "-"),
            ExprType::Plus => write!(f, "+"),
            ExprType::Star => write!(f, "*"),
            ExprType::Eq => write!(f, "="),
            ExprType::EqEq => write!(f, "=="),
            ExprType::Bang => write!(f, "!"),
            ExprType::BangEq => write!(f, "!="),
            ExprType::Less => write!(f, "<"),
            ExprType::LessEq => write!(f, "<="),
            ExprType::Greater => write!(f, ">"),
            ExprType::GreaterEq => write!(f, ">="),
            ExprType::Slash => write!(f, "/"),
            ExprType::Group(_, _) => write!(f, "group"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstToken {
    Number(f64),
    String(String),
    Identifier(String),
    Expr(ExprType),
    Eof,
}

impl From<&LexToken> for AstToken {
    fn from(value: &LexToken) -> Self {
        match value {
            LexToken::Keyword(k) => match k {
                crate::token::KeywordToken::And => AstToken::Expr(ExprType::And),
                crate::token::KeywordToken::Or => AstToken::Expr(ExprType::Or),
                crate::token::KeywordToken::True => AstToken::Expr(ExprType::True),
                crate::token::KeywordToken::False => AstToken::Expr(ExprType::False),
                crate::token::KeywordToken::While => AstToken::Expr(ExprType::While),
                crate::token::KeywordToken::For => AstToken::Expr(ExprType::For),
                crate::token::KeywordToken::If => AstToken::Expr(ExprType::If),
                crate::token::KeywordToken::Return => AstToken::Expr(ExprType::Return),
                crate::token::KeywordToken::Var => AstToken::Expr(ExprType::Var),
                crate::token::KeywordToken::Print => AstToken::Expr(ExprType::Print),
                crate::token::KeywordToken::Nil => AstToken::Expr(ExprType::Nil),
                _ => todo!("from keyword {k}"),
            },
            LexToken::Literal(l) => match l {
                LiteralToken::Less => AstToken::Expr(ExprType::Less),
                LiteralToken::LessEq => AstToken::Expr(ExprType::LessEq),
                LiteralToken::Greater => AstToken::Expr(ExprType::Greater),
                LiteralToken::GreaterEq => AstToken::Expr(ExprType::GreaterEq),
                LiteralToken::EqEq => AstToken::Expr(ExprType::EqEq),
                LiteralToken::BangEq => AstToken::Expr(ExprType::BangEq),
                LiteralToken::Plus => AstToken::Expr(ExprType::Plus),
                LiteralToken::Minus => AstToken::Expr(ExprType::Minus),
                LiteralToken::Star => AstToken::Expr(ExprType::Star),
                LiteralToken::Slash => AstToken::Expr(ExprType::Slash),
                LiteralToken::Dot => AstToken::Expr(ExprType::Dot),
                LiteralToken::Bang => AstToken::Expr(ExprType::Bang),
                // note/todo: using Eof as the group is a hack; need a placeholder; probably box isn't right also, need RefCell?
                LiteralToken::LeftParen => {
                    AstToken::Expr(ExprType::Group('(', Box::new(AstToken::Eof)))
                }
                LiteralToken::RightParen => {
                    AstToken::Expr(ExprType::Group(')', Box::new(AstToken::Eof)))
                }
                LiteralToken::LeftBrace => {
                    AstToken::Expr(ExprType::Group('{', Box::new(AstToken::Eof)))
                }
                LiteralToken::RightBrace => {
                    AstToken::Expr(ExprType::Group('}', Box::new(AstToken::Eof)))
                }
                _ => todo!("from literal token {l}"),
            },
            LexToken::Number { value, .. } => AstToken::Number(*value),
            LexToken::Identifier { value } => AstToken::Identifier(value.clone()),
            LexToken::String { value } => AstToken::String(value.clone()),
            LexToken::Comment => todo!("from comment to ?"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ast {
    Atom(AstToken),
    Cons(Box<Ast>, Vec<Ast>),
}

impl Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fmt = match self {
            Ast::Atom(a) => match a {
                AstToken::Number(n) => codecrafters::format_float(*n).to_string(),
                AstToken::String(s) => s.to_string(),
                AstToken::Identifier(s) => s.to_string(),
                AstToken::Expr(op) => format!("{op}"),
                AstToken::Eof => "".to_string(),
            },
            Ast::Cons(head, rest) => {
                let mut fmt = format!("({head} ");

                for s in rest {
                    fmt.push_str(format!("{s} ").as_str());
                }

                let cons = fmt.trim_end();
                format!("{})", cons)
            }
        };

        write!(f, "{}", fmt.trim_end())
    }
}

#[derive(Debug, thiserror::Error)]
enum ParserError<'a> {
    #[error("[line {line}] Error at {token}: Expect expression.")]
    InvalidExpression { line: usize, token: &'a str },
    #[error("[line {line}] Unexpected EOF.")]
    UnexpectedEof { line: usize },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::KeywordToken;
    use crate::token::LexToken::Keyword;
    use anyhow::bail;
    use LexToken::Literal;

    #[test]
    fn single_digit() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![LexToken::Number {
            raw: "1".to_string(),
            value: 1.0,
        }]);
        let ast = p.parse_expression(0)?;
        match ast {
            Ast::Atom(AstToken::Number(v)) => {
                assert_eq!(v, 1.0);
                assert_eq!(format!("{ast}"), "1.0");

                Ok(())
            }
            _ => bail!("expected: Ast::Atom(AstToken::Number(1.0))"),
        }
    }

    #[test]
    fn simple_add_expression() -> anyhow::Result<()> {
        let tokens = vec![
            LexToken::Number {
                raw: "1".to_string(),
                value: 1.0,
            },
            Literal(LiteralToken::Plus),
            LexToken::Number {
                raw: "2".to_string(),
                value: 2.0,
            },
        ];

        let p = &mut PrattParser::new(tokens);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "(+ 1.0 2.0)");
        Ok(())
    }

    #[test]
    fn true_exp() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![Keyword(KeywordToken::True)]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "true");
        Ok(())
    }

    #[test]
    fn false_exp() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![Keyword(KeywordToken::False)]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "false");
        Ok(())
    }

    #[test]
    fn nil_exp() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![Keyword(KeywordToken::Nil)]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "nil");
        Ok(())
    }

    #[test]
    fn group_group_true_exp() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::LeftParen),
            Literal(LiteralToken::LeftParen),
            Keyword(KeywordToken::True),
            Literal(LiteralToken::RightParen),
            Literal(LiteralToken::RightParen),
        ]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "(group (group true))");
        Ok(())
    }

    #[test]
    fn group_number_exp() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::LeftParen),
            LexToken::Number {
                raw: "1".to_string(),
                value: 1.0,
            },
            Literal(LiteralToken::RightParen),
        ]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "(group 1.0)");
        Ok(())
    }

    #[test]
    fn bang_true() -> anyhow::Result<()> {
        // in: !true
        // out: (! true)
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::Bang),
            Keyword(KeywordToken::True),
        ]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "(! true)");
        Ok(())
    }

    #[test]
    fn bang_bang_true() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::Bang),
            Literal(LiteralToken::Bang),
            Keyword(KeywordToken::True),
        ]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "(! (! true))");
        Ok(())
    }

    #[test]
    fn comparison_ops() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![
            LexToken::Number {
                raw: "40".to_string(),
                value: 40.0,
            },
            Literal(LiteralToken::Less),
            LexToken::Number {
                raw: "121".to_string(),
                value: 121.0,
            },
            Literal(LiteralToken::Less),
            LexToken::Number {
                raw: "202".to_string(),
                value: 202.0,
            },
        ]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "(< (< 40.0 121.0) 202.0)");

        Ok(())
    }

    #[test]
    fn syntax_error_unterminated_group() {
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::LeftParen),
            LexToken::String { value: "baz".to_string() }
        ]);

        assert!(p.parse_expression(0).is_err());
    }

    #[test]
    fn syntax_error_single_plus() {
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::Plus),
        ]);

        assert!(p.parse_expression(0).is_err());
    }
}
