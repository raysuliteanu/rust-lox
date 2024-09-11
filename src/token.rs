use std::fmt::Display;
use crate::codecrafters;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(KeywordToken),
    Literal(LiteralToken),
    Number {
        raw: String,
        value: f64,
    },
    Identifier {
        value: String,
    },
    String {
        value: String,
    },
    Comment,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(t) => write!(f, "{t}"),
            Token::Literal(t) => write!(f, "{t}"),
            Token::Number { raw, value } => write!(f, "NUMBER {raw} {}", codecrafters::format_float(*value)),
            Token::Identifier { value } => write!(f, "IDENTIFIER {value} null"),
            Token::String { value } => write!(f, "STRING \"{value}\" {value}"),
            Token::Comment => {
                // ignore
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    Print,
}

impl Display for KeywordToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeywordToken::And => write!(f, "AND and null"),
            KeywordToken::Class => write!(f, "CLASS class null"),
            KeywordToken::Else => write!(f, "ELSE else null"),
            KeywordToken::False => write!(f, "FALSE false null"),
            KeywordToken::For => write!(f, "FOR for null"),
            KeywordToken::Fun => write!(f, "FUN fun null"),
            KeywordToken::If => write!(f, "IF if null"),
            KeywordToken::Nil => write!(f, "NIL nil null"),
            KeywordToken::Or => write!(f, "OR or null"),
            KeywordToken::Print => write!(f, "PRINT print null"),
            KeywordToken::Return => write!(f, "RETURN return null"),
            KeywordToken::Super => write!(f, "SUPER super null"),
            KeywordToken::This => write!(f, "THIS this null"),
            KeywordToken::True => write!(f, "TRUE true null"),
            KeywordToken::Var => write!(f, "VAR var null"),
            KeywordToken::While => write!(f, "WHILE while null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralToken {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
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
            LiteralToken::Minus => write!(f, "MINUS - null"),
            LiteralToken::Plus => write!(f, "PLUS + null"),
            LiteralToken::SemiColon => write!(f, "SEMICOLON ; null"),
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
