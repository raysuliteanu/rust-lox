use crate::codecrafters;
use miette::{Diagnostic, SourceSpan};
use std::fmt::Display;
use thiserror::Error;

pub struct Lexer<'le> {
    source_file: String,
    source: &'le str,
    offset: usize,
    errors: Vec<miette::Error>,
}

impl<'le> Lexer<'le> {
    pub fn new(source_file: String, source: &'le str) -> Self {
        Self {
            source_file,
            source,
            offset: 0,
            errors: Vec::new(),
        }
    }

    #[cfg(test)]
    pub fn new_from_string(source: &'le str) -> Self {
        Self::new("test".to_string(), source)
    }

    pub fn tokenize(&mut self) -> Result<Vec<LexToken>, miette::Error> {
        let mut tokens = vec![];
        // let mut errors: Vec<miette::Error> = vec![];
        for next in self {
            match next {
                Ok(t) => {
                    println!("{t}");
                    tokens.push(t) 
                },
                Err(e) => {
                    println!("{e}");
                    // errors.push(e);
                }
            }
        }

        // self.errors.append(&mut errors);

        Ok(tokens)
    }

    fn offset(&self) -> usize {
        self.offset
    }

    fn current_line(&self) -> usize {
        self.source[..self.offset].lines().count()
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.offset)
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.source.chars().nth(self.offset);
        self.offset += 1;

        next
    }

    fn tokenize_keyword_or_identifier(&mut self) -> Option<LexToken> {
        let start = self.offset - 1;
        // split_once will "remove" the space if found ... neither part contains the space
        let word =
            match self.source[start..].split_once(|c: char| !(c.is_alphanumeric() || c == '_')) {
                Some((word, _)) => word,
                None => &self.source[start..],
            };

        let token = match word {
            "and" => LexToken::Keyword(KeywordToken::And),
            "class" => LexToken::Keyword(KeywordToken::Class),
            "else" => LexToken::Keyword(KeywordToken::Else),
            "false" => LexToken::Keyword(KeywordToken::False),
            "for" => LexToken::Keyword(KeywordToken::For),
            "fun" => LexToken::Keyword(KeywordToken::Fun),
            "if" => LexToken::Keyword(KeywordToken::If),
            "nil" => LexToken::Keyword(KeywordToken::Nil),
            "or" => LexToken::Keyword(KeywordToken::Or),
            "print" => LexToken::Keyword(KeywordToken::Print),
            "return" => LexToken::Keyword(KeywordToken::Return),
            "super" => LexToken::Keyword(KeywordToken::Super),
            "this" => LexToken::Keyword(KeywordToken::This),
            "true" => LexToken::Keyword(KeywordToken::True),
            "var" => LexToken::Keyword(KeywordToken::Var),
            "while" => LexToken::Keyword(KeywordToken::While),
            _ => LexToken::Identifier {
                value: String::from(&self.source[start..start + word.len()]),
            },
        };

        self.offset += word.len() - 1; // -1 because we added one at the start

        Some(token)
    }

    fn tokenize_number(&mut self) -> Option<LexToken> {
        todo!()
    }

    fn tokenize_op_or_opequal(
        &mut self,
        this: LiteralToken,
        that: LiteralToken,
    ) -> Option<LexToken> {
        self.peek()
            .is_some_and(|c| c == '=')
            .then(|| {
                self.next(); // eat the '='
                LexToken::Literal(that)
            })
            .or(Some(LexToken::Literal(this)))
    }

    fn tokenize_slash_or_comment(&mut self) -> Option<LexToken> {
        todo!()
    }

    fn tokenize_string_literal(&mut self) -> Option<LexToken> {
        todo!()
    }
}

impl<'le> Iterator for Lexer<'le> {
    type Item = Result<LexToken, miette::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let cur_char = self.advance();
            let t = match cur_char {
                Some(c) => match c {
                    c if c.is_whitespace() => continue,
                    '(' => Some(LexToken::Literal(LiteralToken::LeftParen)),
                    ')' => Some(LexToken::Literal(LiteralToken::RightParen)),
                    '{' => Some(LexToken::Literal(LiteralToken::LeftBrace)),
                    '}' => Some(LexToken::Literal(LiteralToken::RightBrace)),
                    ',' => Some(LexToken::Literal(LiteralToken::Comma)),
                    '.' => Some(LexToken::Literal(LiteralToken::Dot)),
                    '+' => Some(LexToken::Literal(LiteralToken::Plus)),
                    ';' => Some(LexToken::Literal(LiteralToken::SemiColon)),
                    '*' => Some(LexToken::Literal(LiteralToken::Star)),
                    '"' => self.tokenize_string_literal(),
                    '/' => self.tokenize_slash_or_comment(),
                    '>' => self.tokenize_op_or_opequal(LiteralToken::GreaterEq, LiteralToken::Greater),
                    '<' => self.tokenize_op_or_opequal(LiteralToken::LessEq, LiteralToken::Less),
                    '=' => self.tokenize_op_or_opequal(LiteralToken::EqEq, LiteralToken::Eq),
                    '!' => self.tokenize_op_or_opequal(LiteralToken::BangEq, LiteralToken::Bang),
                    c if c.is_ascii_digit() => self.tokenize_number(),
                    c if c.is_alphanumeric() || c == '_' => self.tokenize_keyword_or_identifier(),
                    _ => {
                        return Some(Err(InvalidToken {
                            src: self.source.to_string(),
                            err: SourceSpan::from(self.offset()),
                            token: c,
                        }
                            .into()));
                    }
                },
                None => None,
            };
            
            return t.map(Ok);
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("Unexpected token: '{token}'")]
pub struct InvalidToken {
    #[source_code]
    src: String,
    #[label("here")]
    err: SourceSpan,
    token: char,
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof;

#[derive(Debug, Clone, PartialEq)]
pub enum LexToken {
    Keyword(KeywordToken),
    Literal(LiteralToken),
    Number { raw: String, value: f64 },
    Identifier { value: String },
    String { value: String },
    Eof,
}

impl Display for LexToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexToken::Keyword(t) => write!(f, "{t}"),
            LexToken::Literal(t) => write!(f, "{t}"),
            LexToken::Number { raw, value } => {
                write!(f, "NUMBER {raw} {}", codecrafters::format_float(*value))
            }
            LexToken::Identifier { value } => write!(f, "IDENTIFIER {value} null"),
            LexToken::String { value } => write!(f, "STRING \"{value}\" {value}"),
            LexToken::Eof => write!(f, "EOF"),
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::token;
    use itertools::assert_equal;

    #[test]
    fn punctuation() {
        let mut scanner = token::Lexer::new_from_string("/(){};,+-*===<=>=!=<>.!");
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = result.unwrap();
        let expected = vec![
            LexToken::Literal(LiteralToken::Slash),
            LexToken::Literal(LiteralToken::LeftParen),
            LexToken::Literal(LiteralToken::RightParen),
            LexToken::Literal(LiteralToken::LeftBrace),
            LexToken::Literal(LiteralToken::RightBrace),
            LexToken::Literal(LiteralToken::SemiColon),
            LexToken::Literal(LiteralToken::Comma),
            LexToken::Literal(LiteralToken::Plus),
            LexToken::Literal(LiteralToken::Minus),
            LexToken::Literal(LiteralToken::Star),
            LexToken::Literal(LiteralToken::EqEq),
            LexToken::Literal(LiteralToken::Eq),
            LexToken::Literal(LiteralToken::LessEq),
            LexToken::Literal(LiteralToken::GreaterEq),
            LexToken::Literal(LiteralToken::BangEq),
            LexToken::Literal(LiteralToken::Less),
            LexToken::Literal(LiteralToken::Greater),
            LexToken::Literal(LiteralToken::Dot),
            LexToken::Literal(LiteralToken::Bang),
        ];

        check(actual, expected);
    }

    #[test]
    fn keywords() {
        let keywords =
            "and class else false for fun if nil or print return super this true var while";
        let mut scanner = token::Lexer::new_from_string(keywords);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = result.unwrap();
        let expected = vec![
            LexToken::Keyword(KeywordToken::And),
            LexToken::Keyword(KeywordToken::Class),
            LexToken::Keyword(KeywordToken::Else),
            LexToken::Keyword(KeywordToken::False),
            LexToken::Keyword(KeywordToken::For),
            LexToken::Keyword(KeywordToken::Fun),
            LexToken::Keyword(KeywordToken::If),
            LexToken::Keyword(KeywordToken::Nil),
            LexToken::Keyword(KeywordToken::Or),
            LexToken::Keyword(KeywordToken::Print),
            LexToken::Keyword(KeywordToken::Return),
            LexToken::Keyword(KeywordToken::Super),
            LexToken::Keyword(KeywordToken::This),
            LexToken::Keyword(KeywordToken::True),
            LexToken::Keyword(KeywordToken::Var),
            LexToken::Keyword(KeywordToken::While),
        ];

        check(actual, expected);
    }

    #[test]
    fn string_literals() {
        let input = "\"some string value\"";
        let mut scanner = token::Lexer::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = result.unwrap();
        let expected = vec![LexToken::String {
            value: "some string value".to_string(),
        }];

        check(actual, expected);
    }

    #[test]
    fn string_literals_with_other_stuff() {
        let input = "var x = \"some string value\";";
        let mut scanner = token::Lexer::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = result.unwrap();
        let expected = vec![
            LexToken::Keyword(KeywordToken::Var),
            LexToken::Identifier {
                value: "x".to_string(),
            },
            LexToken::Literal(LiteralToken::Eq),
            LexToken::String {
                value: "some string value".to_string(),
            },
            LexToken::Literal(LiteralToken::SemiColon),
        ];

        check(actual, expected);
    }

    #[test]
    fn addition_and_subtraction() {
        let input = "1 + 2 - 3";
        let mut scanner = token::Lexer::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = result.unwrap();
        let expected = vec![
            LexToken::Number {
                raw: "1".to_string(),
                value: 1.0,
            },
            LexToken::Literal(LiteralToken::Plus),
            LexToken::Number {
                raw: "2".to_string(),
                value: 2.0,
            },
            LexToken::Literal(LiteralToken::Minus),
            LexToken::Number {
                raw: "3".to_string(),
                value: 3.0,
            },
        ];

        check(actual, expected);
    }

    #[test]
    fn numbers() {
        let input = "123 123.456 .456 123. 42.42 -67";
        let mut scanner = token::Lexer::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = result.unwrap();
        let expected = vec![
            LexToken::Number {
                raw: "123".to_string(),
                value: 123.0,
            },
            LexToken::Number {
                raw: "123.456".to_string(),
                value: 123.456,
            },
            LexToken::Literal(LiteralToken::Dot),
            LexToken::Number {
                raw: "456".to_string(),
                value: 456.0,
            },
            LexToken::Number {
                raw: "123".to_string(),
                value: 123.0,
            },
            LexToken::Literal(LiteralToken::Dot),
            LexToken::Number {
                raw: "42.42".to_string(),
                value: 42.42,
            },
            LexToken::Number {
                raw: "-67".to_string(),
                value: -67.0,
            },
        ];

        check(actual, expected);
    }

    #[test]
    fn identifiers() {
        let input = "(foo, bar, baz)";
        let mut scanner = token::Lexer::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = result.unwrap();
        let expected = vec![
            LexToken::Literal(LiteralToken::LeftParen),
            LexToken::Identifier {
                value: "foo".to_string(),
            },
            LexToken::Literal(LiteralToken::Comma),
            LexToken::Identifier {
                value: "bar".to_string(),
            },
            LexToken::Literal(LiteralToken::Comma),
            LexToken::Identifier {
                value: "baz".to_string(),
            },
            LexToken::Literal(LiteralToken::RightParen),
        ];

        check(actual, expected);
    }

    fn check(actual: Vec<LexToken>, expected: Vec<LexToken>) {
        assert_eq!(actual.len(), expected.len());
        assert_equal(actual, expected);
    }
}
