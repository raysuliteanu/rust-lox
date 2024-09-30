use crate::codecrafters;
use crate::error::{InterpreterResult, ParserError};
use crate::token::{LexToken, LiteralToken};
use log::{debug, trace};
use std::fmt::{self, Display, Formatter};

pub struct PrattParser {
    tokens: Vec<LexToken>,
    current: usize,
}

impl PrattParser {
    pub fn new(tokens: Vec<LexToken>) -> Self {
        PrattParser { tokens, current: 0 }
    }

    pub fn parse(mut self) -> InterpreterResult<Ast> {
        self.parse_expression(0)
    }

    fn parse_expression(&mut self, min_bp: u8) -> InterpreterResult<Ast> {
        trace!("parse_expression(${min_bp})");

        let mut lhs = if self.expect_token(LexToken::Literal(LiteralToken::LeftParen)) {
            trace!("matched '('");
            self.advance();

            let group = self.parse_expression(0)?;

            debug!("parsed: {group}");

            if self.expect_token(LexToken::Literal(LiteralToken::RightParen)) {
                self.advance();
                Ast::Cons(Box::new(Ast::Atom(AstToken::GroupStart)), vec![group])
            } else {
                return Err(ParserError::InvalidExpression {
                    // todo: line number needs to be set correctly
                    line: 0,
                    token: ")",
                }
                .into());
            }
        } else {
            match self.next_token() {
                Some(ast) => match ast {
                    Ast::Atom(AstToken::Expr(_)) => {
                        if let Some((_, r_bp)) = self.prefix_binding_power(&ast) {
                            let rhs = self.parse_expression(r_bp)?;
                            Ast::Cons(Box::from(ast), vec![rhs])
                        } else {
                            ast
                        }
                    }
                    Ast::Atom(AstToken::Number(_))
                    | Ast::Atom(AstToken::String(_))
                    | Ast::Atom(AstToken::Identifier(_))
                    | Ast::Atom(AstToken::Eof) => ast,
                    _ => {
                        return Err(ParserError::InvalidExpression {
                            line: 0,
                            token: ")", // todo: right now that's the issue but need to be able to have the actual token
                        }
                        .into())
                    }
                },
                None => return Err(ParserError::UnexpectedEof { line: 0 }.into()),
            }
        };

        #[allow(clippy::while_let_loop)]
        loop {
            let ast = if self.expect_token(LexToken::Literal(LiteralToken::LeftParen)) {
                Some(self.parse_expression(min_bp)?)
            } else {
                self.peek_token()
            };

            let peek_token = match ast {
                Some(t) => match t {
                    Ast::Atom(AstToken::Expr(_)) => t,
                    Ast::Atom(AstToken::Eof) | Ast::Atom(AstToken::GroupEnd) => break,
                    bt => panic!("bad token: {bt:?}"),
                },
                None => {
                    trace!("next token: None");
                    break;
                }
            };

            debug!("peek_token = {peek_token}");

            if let Some((l_bp, ())) = self.postfix_binding_power(&lhs) {
                debug!("l_bp = {l_bp}");

                if l_bp < min_bp {
                    trace!("l_bp({l_bp}) < min_bp({min_bp})");
                    break;
                }

                let nt = self.next_token();
                debug!("discarding next token = {nt:?}");

                lhs = Ast::Cons(Box::from(lhs), vec![peek_token]);

                debug!("lhs = {lhs}");

                continue;
            }

            if let Some((l_bp, r_bp)) = self.infix_binding_power(&peek_token) {
                debug!("(l_bp({l_bp}), r_bp({r_bp}))");

                if l_bp < min_bp {
                    trace!("l_bp({l_bp}) < min_bp({min_bp})");
                    break;
                }

                let nt = self.next_token();
                debug!("discarding next token = {nt:?}");

                let rhs = self.parse_expression(r_bp)?;

                lhs = Ast::Cons(Box::from(peek_token), vec![lhs, rhs]);

                debug!("lhs = {lhs}");

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

    fn advance(&mut self) {
        trace!("advancing from {} to {}", self.current, self.current + 1);
        self.current += 1;
    }

    fn expect_token(&mut self, lex_token: LexToken) -> bool {
        trace!("expect_token({lex_token}");
        self.tokens
            .get(self.current)
            .is_some_and(|t| *t == lex_token)
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
    GroupStart,
    GroupEnd,
}

impl Display for AstToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AstToken::Number(n) => write!(f, "{}", codecrafters::format_float(*n)),
            AstToken::Identifier(s) | AstToken::String(s) => write!(f, "{s}"),
            AstToken::Expr(op) => write!(f, "{op}"),
            AstToken::Eof => write!(f, "<eof>"),
            AstToken::GroupStart => write!(f, "<gs>"),
            AstToken::GroupEnd => write!(f, "<ge>"),
        }
    }
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
                LiteralToken::LeftParen | LiteralToken::LeftBrace => AstToken::GroupStart,
                LiteralToken::RightParen | LiteralToken::RightBrace => AstToken::GroupEnd,
                _ => todo!("from literal token {l}"),
            },
            LexToken::Number { value, .. } => AstToken::Number(*value),
            LexToken::Identifier { value } => AstToken::Identifier(value.clone()),
            LexToken::String { value } => AstToken::String(value.clone()),
            LexToken::Comment => todo!("from comment to ?"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    Atom(AstToken),
    Cons(Box<Ast>, Vec<Ast>),
}

impl Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fmt = match self {
            Ast::Atom(a) => format!("{a}"),
            Ast::Cons(head, rest) => {
                let mut fmt = "(".to_string();

                if **head != Ast::Atom(AstToken::GroupStart) {
                    fmt.push_str(format!("{head} ").as_str());
                } else {
                    fmt.push_str("group ");
                }

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::KeywordToken;
    use crate::token::LexToken::{Keyword, Number};
    use LexToken::Literal;

    #[test]
    fn single_digit() -> InterpreterResult<()> {
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
            _ => panic!("expected: Ast::Atom(AstToken::Number(1.0))"),
        }
    }

    #[test]
    fn simple_add_expression() -> InterpreterResult<()> {
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
    fn true_exp() -> InterpreterResult<()> {
        let p = &mut PrattParser::new(vec![Keyword(KeywordToken::True)]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "true");
        Ok(())
    }

    #[test]
    fn false_exp() -> InterpreterResult<()> {
        let p = &mut PrattParser::new(vec![Keyword(KeywordToken::False)]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "false");
        Ok(())
    }

    #[test]
    fn nil_exp() -> InterpreterResult<()> {
        let p = &mut PrattParser::new(vec![Keyword(KeywordToken::Nil)]);
        let ast = p.parse_expression(0)?;
        assert_eq!(format!("{ast}"), "nil");
        Ok(())
    }

    #[test]
    fn group_group_true_exp() -> InterpreterResult<()> {
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
    fn group_number_exp() -> InterpreterResult<()> {
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
    fn bang_true() -> InterpreterResult<()> {
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
    fn bang_bang_true() -> InterpreterResult<()> {
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
    fn comparison_ops() -> InterpreterResult<()> {
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
            LexToken::String {
                value: "baz".to_string(),
            },
        ]);

        assert!(p.parse_expression(0).is_err());
    }

    #[test]
    fn syntax_error_unterminated_group_with_identifier() {
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::LeftParen),
            LexToken::Identifier {
                value: "baz".to_string(),
            },
        ]);

        assert!(p.parse_expression(0).is_err());
    }

    #[test]
    fn syntax_error_single_plus() {
        let p = &mut PrattParser::new(vec![Literal(LiteralToken::Plus)]);

        assert!(p.parse_expression(0).is_err());
    }

    #[test]
    fn syntax_error_invalid_addition_expr() {
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::LeftParen),
            Number { value: 27.0, raw: "27".to_string() },
            Literal(LiteralToken::Plus),
            Literal(LiteralToken::RightParen),
        ]);

        assert!(p.parse_expression(0).is_err());
    }
    
    #[test]
    fn complex_expression() {
        //  (54 != 58) == ((-93 + 11) >= (47 * 11))
        let p = &mut PrattParser::new(vec![
            Literal(LiteralToken::LeftParen),
            Number { value: 54.0, raw: "54".to_string() },
            Literal(LiteralToken::BangEq),
            Number { value: 58.0, raw: "58".to_string() },
            Literal(LiteralToken::RightParen),
            Literal(LiteralToken::EqEq),
            Literal(LiteralToken::LeftParen),
            Literal(LiteralToken::LeftParen),
            Number { value: -93.0, raw: "-93".to_string() },
            Literal(LiteralToken::Plus),
            Number { value: 11.0, raw: "11".to_string() },
            Literal(LiteralToken::RightParen),
            Literal(LiteralToken::GreaterEq),
            Literal(LiteralToken::LeftParen),
            Number { value: 47.0, raw: "47".to_string() },
            Literal(LiteralToken::Star),
            Number { value: 11.0, raw: "11".to_string() },
            Literal(LiteralToken::RightParen),
            Literal(LiteralToken::RightParen),
        ]);

        // expected: (== (group (!= 54.0 58.0)) (group (>= (group (+ (- 93.0) 11.0)) (group (* 47.0 11.0)))))
        // got     : (== (group (!= 54.0 58.0)) (group (>= (group (+ -93.0 11.0)) (group (* 47.0 11.0)))))
        assert!(p.parse_expression(0).is_err());
    }
}
