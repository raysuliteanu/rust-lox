use crate::token::{LiteralToken, Token};
use std::fmt::{self, Display};

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
                AstToken::Number(_) | AstToken::Op(_) => Ast::Atom(value),
                _ => Ast::Atom(AstToken::Eof),
            },
            None => panic!("bad token stream"),
        };

        while let Some(op) = self.next_token() {
            let r_bp = if let Some((l_bp, r_bp)) = self.infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }

                r_bp
            } else {
                todo!("infix_binding_power() found invalid op: {op}");
            };

            // let _ = self.next_token();

            let rhs = self.parse_expression(r_bp)?;

            lhs = Ast::Cons(op, vec![lhs, rhs]);
        }

        Ok(lhs)
    }

    fn next_token(&mut self) -> Option<AstToken> {
        let token = self.tokens.get(self.current).map(|t| t.into());
        self.current += 1;
        token
    }

    fn infix_binding_power(&mut self, op: &AstToken) -> Option<(u8, u8)> {
        match op {
            AstToken::Op(op) => match op {
                OpType::EqEq => Some((2, 1)),
                OpType::Plus | OpType::Minus => Some((5, 6)),
                OpType::Star | OpType::Slash => Some((7, 8)),
                OpType::Dot => Some((14, 13)),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum OpType {
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
}

impl Display for OpType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpType::And => write!(f, "and"),
            OpType::False => write!(f, "false"),
            OpType::For => write!(f, "for"),
            OpType::If => write!(f, "if"),
            OpType::Or => write!(f, "or"),
            OpType::Print => write!(f, "print"),
            OpType::Return => write!(f, "return"),
            OpType::True => write!(f, "true"),
            OpType::Var => write!(f, "var"),
            OpType::While => write!(f, "while"),
            OpType::Nil => write!(f, "nil"),
            OpType::Dot => write!(f, "."),
            OpType::Minus => write!(f, "-"),
            OpType::Plus => write!(f, "+"),
            OpType::Star => write!(f, "*"),
            OpType::Eq => write!(f, "="),
            OpType::EqEq => write!(f, "=="),
            OpType::Bang => write!(f, "!"),
            OpType::BangEq => write!(f, "!="),
            OpType::Less => write!(f, "<"),
            OpType::LessEq => write!(f, "<="),
            OpType::Greater => write!(f, ">"),
            OpType::GreaterEq => write!(f, ">="),
            OpType::Slash => write!(f, "/"),
        }
    }
}
#[derive(Debug, Clone)]
pub enum AstToken {
    Number(f64),
    String(String),
    Identifier(String),
    Op(OpType),
    Eof,
}

impl Display for AstToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstToken::Number(n) => write!(f, "{n}"),
            AstToken::String(s) => write!(f, "{s}"),
            AstToken::Identifier(s) => write!(f, "{s}"),
            AstToken::Op(op) => write!(f, "{op}"),
            AstToken::Eof => Ok(()),
        }
    }
}

impl From<&Token> for AstToken {
    fn from(value: &Token) -> Self {
        match value {
            Token::Keyword(k) => match k {
                crate::token::KeywordToken::And => AstToken::Op(OpType::And),
                crate::token::KeywordToken::Or => AstToken::Op(OpType::Or),
                crate::token::KeywordToken::True => AstToken::Op(OpType::True),
                crate::token::KeywordToken::False => AstToken::Op(OpType::False),
                crate::token::KeywordToken::While => AstToken::Op(OpType::While),
                crate::token::KeywordToken::For => AstToken::Op(OpType::For),
                crate::token::KeywordToken::If => AstToken::Op(OpType::If),
                crate::token::KeywordToken::Return => AstToken::Op(OpType::Return),
                crate::token::KeywordToken::Var => AstToken::Op(OpType::Var),
                crate::token::KeywordToken::Print => AstToken::Op(OpType::Print),
                crate::token::KeywordToken::Nil => AstToken::Op(OpType::Nil),
                _ => todo!("from keyword {k}"),
            },
            Token::Literal(l) => match l {
                LiteralToken::EqEq => AstToken::Op(OpType::EqEq),
                LiteralToken::Plus => AstToken::Op(OpType::Plus),
                LiteralToken::Minus => AstToken::Op(OpType::Minus),
                LiteralToken::Star => AstToken::Op(OpType::Star),
                LiteralToken::Slash => AstToken::Op(OpType::Slash),
                LiteralToken::Dot => AstToken::Op(OpType::Dot),
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
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
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
