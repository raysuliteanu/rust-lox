use std::fmt::Display;

#[derive(Debug)]
pub enum Token {
    Keyword(KeywordToken),
    Literal(LiteralToken),
    Number(f64),
    Identifier {
        // index into source buffer
        offset: usize,
        length: usize,
    },
    String {
        // index into source buffer
        offset: usize,
        length: usize,
    },
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(t) => todo!(),
            Token::Literal(t) => write!(f, "{t}"),
            Token::Number(n) => write!(f, "{n}"),
            Token::Identifier { offset, length } => todo!(),
            Token::String { offset, length } => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum KeywordToken {
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug)]
pub enum LiteralToken {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Dash,
    Plus,
    SemiColon,
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
}

impl Display for LiteralToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralToken::LeftParen => write!(f, "LEFT_PAREN ( null"),
            LiteralToken::RightParen => write!(f, "RIGHT_PAREN ) null"),
            LiteralToken::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            LiteralToken::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            LiteralToken::Comma => write!(f, "COMMA , null"),
            LiteralToken::Dot => write!(f, "DOT . null"),
            LiteralToken::Dash => write!(f, "DASH - null"),
            LiteralToken::Plus => write!(f, "PLUS + null"),
            LiteralToken::SemiColon => write!(f, "SEMI_COLON : null"),
            LiteralToken::Star => write!(f, "STAR * null"),
            LiteralToken::Eq => write!(f, "EQUAL = null"),
            LiteralToken::EqEq => write!(f, "EQUAL_EQUAL == null"),
            LiteralToken::Bang => write!(f, "BANG ! null"),
            LiteralToken::BangEq => write!(f, "BANG_EQUAL != null"),
            LiteralToken::Less => write!(f, "LESS < null"),
            LiteralToken::LessEq => write!(f, "LESS_EQUAL <= null"),
            LiteralToken::Greater => write!(f, "GREATER > null"),
            LiteralToken::GreaterEq => write!(f, "GREATER_EQUAL >= null"),
            LiteralToken::Slash => write!(f, "SLASH / null"),
        }
    }
}
