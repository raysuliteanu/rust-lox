use crate::token::{LiteralToken, Token};
use std::fmt::{self, Display};
use crate::codecrafters;

pub struct PrattParser {
    tokens: Vec<Token>,
    current: usize,
}

impl PrattParser {
    pub fn new(tokens: Vec<Token>) -> Self {
        PrattParser {
            tokens,
            current: 0,
        }
    }

    pub fn parse(mut self) -> anyhow::Result<Ast> {
        self.parse_expression(0)
    }

    fn parse_expression(&mut self, min_bp: u8) -> anyhow::Result<Ast> {
        let mut lhs = match self.next_token() {
            Some(value) => match value {
                AstToken::Expr(ExprType::Group('(', _)) => {
                    let group = self.parse_expression(0)?;
                    eprintln!("group: {group}");
                    // eat closing ')'
                    self.next_token();
                    
                    // assert_eq!(self.next_token().unwrap(), AstToken::Expr(ExprType::Group(')', _)));
                    // AstToken::Expr(Group('(', Box::new(group_expr)))

                    Ast::Cons(value, vec![group])
                    
                }
                AstToken::String(_) | AstToken::Number(_) | AstToken::Expr(_) => Ast::Atom(value),
                _ => Ast::Atom(AstToken::Eof),
            },
            None => panic!("bad token stream"),
        };

        loop {
            let token = match self.peek_token() {
                Some(value) => match value {
                    AstToken::Eof => break,
                    AstToken::Expr(_) => value,
                    t => panic!("bad token: {:?}", t),
                },
                None => break,
            };

            if let Some((l_bp, ())) = self.postfix_binding_power(&token) {
                if l_bp < min_bp {
                    break;
                }

                self.next_token();

                lhs = Ast::Cons(token, vec![lhs]);
                continue;
            }

            if let Some((l_bp, r_bp)) = self.infix_binding_power(&token) {
                if l_bp < min_bp {
                    break;
                }

                self.next_token();

                let rhs = self.parse_expression(r_bp)?;

                lhs = Ast::Cons(token, vec![lhs, rhs]);
                
                continue;
            };
            
            break;
        }

        Ok(lhs)
    }

    fn next_token(&mut self) -> Option<AstToken> {
        let token = self.tokens.get(self.current).map(|t| t.into());
        self.current += 1;
        token
    }

    fn peek_token(&mut self) -> Option<AstToken> {
        self.tokens.get(self.current + 1).map(|t| t.into())
    }

    fn infix_binding_power(&mut self, token: &AstToken) -> Option<(u8, u8)> {
        eprintln!("infix_binding_power({token})");
        match token {
            AstToken::Expr(op) => match op {
                ExprType::EqEq => Some((2, 1)),
                ExprType::Plus | ExprType::Minus => Some((5, 6)),
                ExprType::Star | ExprType::Slash => Some((7, 8)),
                ExprType::Dot => Some((14, 13)),
                _ => None,
            },
            _ => None,
        }
    }

    fn postfix_binding_power(&self, token: &AstToken) -> Option<(u8, ())> {
        match token {
            AstToken::Expr(ExprType::Bang) => Some((7, ())),
            _ => None,
        }
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

impl Display for AstToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstToken::Number(n) => write!(f, "{}", codecrafters::format_float(*n)),
            AstToken::String(s) => write!(f, "{s}"),
            AstToken::Identifier(s) => write!(f, "{s}"),
            AstToken::Expr(op) => write!(f, "{op}"),
            AstToken::Eof => Ok(()),
        }
    }
}

impl From<&Token> for AstToken {
    fn from(value: &Token) -> Self {
        match value {
            Token::Keyword(k) => match k {
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
            Token::Literal(l) => match l {
                LiteralToken::EqEq => AstToken::Expr(ExprType::EqEq),
                LiteralToken::Plus => AstToken::Expr(ExprType::Plus),
                LiteralToken::Minus => AstToken::Expr(ExprType::Minus),
                LiteralToken::Star => AstToken::Expr(ExprType::Star),
                LiteralToken::Slash => AstToken::Expr(ExprType::Slash),
                LiteralToken::Dot => AstToken::Expr(ExprType::Dot),
                // note/todo: using Eof as the group is a hack; need a placeholder; probably box isn't right also, need RefCell?
                LiteralToken::LeftParen => AstToken::Expr(ExprType::Group('(', Box::new(AstToken::Eof))),
                LiteralToken::RightParen => AstToken::Expr(ExprType::Group(')', Box::new(AstToken::Eof))),
                LiteralToken::LeftBrace => AstToken::Expr(ExprType::Group('{', Box::new(AstToken::Eof))),
                LiteralToken::RightBrace => AstToken::Expr(ExprType::Group('}', Box::new(AstToken::Eof))),
                _ => todo!("from literal token {l}"),
            },
            Token::Number { value, .. } => AstToken::Number(*value),
            Token::Identifier { value } => AstToken::Identifier(value.clone()),
            Token::String { value } => AstToken::String(value.clone()),
            Token::Comment => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ast {
    Atom(AstToken),
    Cons(AstToken, Vec<Ast>),
}

impl Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ast::Atom(i) => write!(f, "{}", i),
            Ast::Cons(head, rest) => {
                write!(f, "({} ", head)?;
                for s in rest {
                    write!(f, "{}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::bail;
    use crate::token::KeywordToken;
    use crate::token::Token::Keyword;
    use super::*;

    #[test]
    fn single_digit() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![
            Token::Number {
                raw: "1".to_string(),
                value: 1.0,
            }]);
        match p.parse_expression(0)? {
            Ast::Atom(AstToken::Number(v)) => {
                assert_eq!(v, 1.0);
                Ok(())
            }
            _ => bail!("expected: Ast::Atom(AstToken::Number(1.0))"),
        }
    }

    #[test]
    fn simple_add_expression() -> anyhow::Result<()> {
        let tokens = vec![
            Token::Number { raw: "1".to_string(), value: 1.0 },
            Token::Literal(LiteralToken::Plus),
            Token::Number { raw: "2".to_string(), value: 2.0 },
        ];

        let p = &mut PrattParser::new(tokens);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "(+ 1 2)");
        Ok(())
    }

    #[test]
    fn true_exp() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![
            Keyword(KeywordToken::True),
        ]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "true");
        Ok(())
    }

    #[test]
    fn false_exp() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![
            Keyword(KeywordToken::False),
        ]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "false");
        Ok(())
    }

    #[test]
    fn nil_exp() -> anyhow::Result<()> {
        let p = &mut PrattParser::new(vec![
            Keyword(KeywordToken::Nil),
        ]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "nil");
        Ok(())
    }
}
