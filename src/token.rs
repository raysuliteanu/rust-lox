use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(KeywordToken),
    Literal(LiteralToken),
    Number {
        raw: String,
        value: f64,
    },
    Identifier {
        // index into source buffer
        // todo: make line string
        offset: usize,
        length: usize,
    },
    String {
        value: String,
    },
    Comment,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // this is for codecrafters specifically
        fn format_float(number: f64) -> String {
            // Use high precision initially
            let mut formatted = format!("{:.10}", number);
            // Remove trailing zeros
            formatted = formatted.trim_end_matches('0').to_string();
            // ensure all formatted numbers end in ".0"
            if formatted.ends_with('.') {
                formatted.push('0');
            }

            formatted
        }

        match self {
            Token::Keyword(t) => write!(f, "{t}"),
            Token::Literal(t) => write!(f, "{t}"),
            Token::Number { raw, value } => write!(f, "NUMBER {raw} {}", format_float(*value)),
            Token::Identifier {
                offset: _,
                length: _,
            } => {
                write!(f, "IDENTIFIER")
            }
            Token::String { value } => {
                write!(f, "STRING \"{value}\" {value}")
            }
            Token::Comment => {
                // ignore
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq)]
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
            KeywordToken::Return => write!(f, "RETURN return null"),
            KeywordToken::Super => write!(f, "SUPER super null"),
            KeywordToken::This => write!(f, "THIS this null"),
            KeywordToken::True => write!(f, "TRUE true null"),
            KeywordToken::Var => write!(f, "VAR var null"),
            KeywordToken::While => write!(f, "WHILE while null"),
        }
    }
}

#[derive(Debug, PartialEq)]
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
