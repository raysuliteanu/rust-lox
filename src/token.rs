use miette::{Diagnostic, SourceSpan};
use miette::Result as MietteResult;
use std::fmt::Display;
use strum::{EnumMessage, IntoStaticStr};
use thiserror::Error;

macro_rules! number_token {
    ($l:ident, $v:ident) => {
        Token::Number { raw: String::from($l), value: $v, }
    };
    ($l:expr, $v:ident) => {
        Token::Number { raw: String::from($l), value: $v, }
    };
    ($l:expr, $v:expr) => {
        Token::Number { raw: String::from($l), value: $v, }
    };
    ($l:ident, $v:expr) => {
        Token::Number { raw: String::from($l), value: $v, }
    };
}

macro_rules! keyword_token {
    ($k:ident) => {
        match ($k) {
            "and" => Some(Token::Keyword(KeywordKind::And)),
            "class" => Some(Token::Keyword(KeywordKind::Class)),
            "else" => Some(Token::Keyword(KeywordKind::Else)),
            "false" => Some(Token::Keyword(KeywordKind::False)),
            "for" => Some(Token::Keyword(KeywordKind::For)),
            "fun" => Some(Token::Keyword(KeywordKind::Fun)),
            "if" => Some(Token::Keyword(KeywordKind::If)),
            "nil" => Some(Token::Keyword(KeywordKind::Nil)),
            "or" => Some(Token::Keyword(KeywordKind::Or)),
            "print" => Some(Token::Keyword(KeywordKind::Print)),
            "return" => Some(Token::Keyword(KeywordKind::Return)),
            "super" => Some(Token::Keyword(KeywordKind::Super)),
            "this" => Some(Token::Keyword(KeywordKind::This)),
            "true" => Some(Token::Keyword(KeywordKind::True)),
            "var" => Some(Token::Keyword(KeywordKind::Var)),
            "while" => Some(Token::Keyword(KeywordKind::While)),
        _ => None,
        }
     };
}

macro_rules! literal_token {
    ($k:literal) => {
        match $k {
            '/' => Token::Literal(LiteralKind::Slash),
            '(' => Token::Literal(LiteralKind::LeftParen),
            ')' => Token::Literal(LiteralKind::RightParen),
            '{' => Token::Literal(LiteralKind::LeftBrace),
            '}' => Token::Literal(LiteralKind::RightBrace),
            ';' => Token::Literal(LiteralKind::SemiColon),
            ',' => Token::Literal(LiteralKind::Comma),
            '+' => Token::Literal(LiteralKind::Plus),
            '-' => Token::Literal(LiteralKind::Minus),
            '*' => Token::Literal(LiteralKind::Star),
            '=' => Token::Literal(LiteralKind::Eq),
            '<' => Token::Literal(LiteralKind::Less),
            '>' => Token::Literal(LiteralKind::Greater),
            '.' => Token::Literal(LiteralKind::Dot),
            '!' => Token::Literal(LiteralKind::Bang),
            _ => panic!("must be single character literal"),
        }
    };
}

// TODO: can't use str in const or static, so figure out the macros to deal
const TOKEN_SLASH: Token = literal_token!('/');
const TOKEN_LEFT_PAREN: Token = literal_token!('(');
const TOKEN_RIGHT_PAREN: Token = literal_token!(')');
const TOKEN_LEFT_BRACE: Token = literal_token!('{');
const TOKEN_RIGHT_BRACE: Token = literal_token!('}');
const TOKEN_SEMICOLON: Token = literal_token!(';');
const TOKEN_COMMA: Token = literal_token!(',');
const TOKEN_PLUS: Token = literal_token!('+');
const TOKEN_MINUS: Token = literal_token!('-');
const TOKEN_STAR: Token = literal_token!('*');
const TOKEN_DOT: Token = literal_token!('.');
const TOKEN_EQ: Token = literal_token!('=');
const TOKEN_EQ_EQ: Token = Token::Literal(LiteralKind::EqEq);
const TOKEN_LESS: Token = literal_token!('<');
const TOKEN_LESS_EQ: Token = Token::Literal(LiteralKind::LessEq);
const TOKEN_GREATER: Token = literal_token!('>');
const TOKEN_GREATER_EQ: Token = Token::Literal(LiteralKind::GreaterEq);
const TOKEN_BANG: Token = literal_token!('!');
const TOKEN_BANG_EQ: Token = Token::Literal(LiteralKind::BangEq);

pub struct Lexer<'le> {
    source: &'le str,
    offset: usize,
}

impl<'le> Lexer<'le> {
    pub fn new(source: &'le str) -> Self {
        Self {
            source,
            offset: 0,
        }
    }

    pub fn tokenize(self) -> Result<(), miette::Error> {
        for next in self {
            match next {
                Ok(t) => {
                    println!("{t}");
                }
                Err(e) => {
                    eprintln!("{:?}", e);
                }
            }
        }

        println!("EOF  null");

        Ok(())
    }

    fn offset(&self) -> usize {
        self.offset
    }

    fn peek(&mut self) -> Option<char> {
        self.source.chars().nth(self.offset)
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.source.chars().nth(self.offset)?;
        self.offset += 1;

        Some(next)
    }

    fn tokenize_keyword_or_identifier(&mut self) -> Option<MietteResult<Token>> {
        let start = self.offset - 1;
        // split_once will "remove" the space if found ... neither part contains the space
        let word =
            match self.source[start..].split_once(|c: char| !(c.is_alphanumeric() || c == '_')) {
                Some((word, _)) => word,
                None => &self.source[start..],
            };

        let token = keyword_token!(word).unwrap_or(
            Token::Identifier {
                value: String::from(&self.source[start..start + word.len()]),
            }
        );

        self.offset += word.len() - 1;

        Some(Ok(token))
    }

    fn tokenize_number(&mut self) -> Option<MietteResult<Token>> {
        let start = self.offset - 1;

        // Find the index in the source of the first char that's *not* 0-9 or .
        let non_digit_idx = self.source[start..]
            .find(|c| !matches!(c, '.' | '0'..='9'))
            .unwrap_or(self.source.len() - start);

        let mut num_literal = &self.source[start..start + non_digit_idx];

        // There are 3 possibilities now. The number can be one of
        // 1. just digits e.g. 123
        // 2. digits plus a trailing . e.g. 123.
        // 3. digits before and after a . e.g. 123.45
        // So doing the splitn(3, '.') will result in 
        // 1. Some, None, None => the _ case 
        // 2. Some, Some, None => the 123. case
        // 3. Some, Some, Some => the 123.45 case
        let mut split = num_literal.splitn(3, '.');
        match (split.next(), split.next(), split.next()) {
            (Some(first), Some(second), Some(_)) => {
                num_literal = &num_literal[..first.len() + 1 + second.len()]; // +1 for the dot sep
            }
            (Some(first), Some(second), None) => {
                if second.is_empty() {
                    num_literal = &num_literal[..first.len()];
                }
            }
            _ => {}
        }

        let value = match num_literal.parse() {
            Ok(value) => value,
            Err(_e) => {
                return Some(Err(InvalidToken {
                    src: self.source.to_string(),
                    span: SourceSpan::new(start.into(), num_literal.len()),
                }
                .into()));
            }
        };

        self.offset = start + num_literal.len();

        Some(Ok(number_token!(num_literal, value)))
    }

    fn tokenize_op_or_opequal(&mut self, op: Token, op_eq: Token) -> Option<MietteResult<Token>> {
        self.peek()
            .is_some_and(|c| c == '=')
            .then(|| {
                assert_eq!(self.advance(), Some('=')); // eat the '='
                Ok(op_eq)
            })
            .or(Some(Ok(op)))
    }

    fn tokenize_string_literal(&mut self) -> Option<MietteResult<Token>> {
        let offset = self.offset();
        if let Some(length) = self.source[offset..].find('"') {
            self.offset += length + 1;
            Some(Ok(Token::String {
                value: String::from(&self.source[offset..offset + length]),
            }))
        } else {
            let e = UnterminatedString {
                src: self.source.to_string(),
                span: SourceSpan::from(self.offset().saturating_sub(1)),
            };
            self.offset = self.source.len();
            Some(Err(e.into()))
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, miette::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let cur_char = self.advance();
            let t = match cur_char {
                Some(c) => match c {
                    c if c.is_whitespace() => continue,
                    '(' => Some(Ok(TOKEN_LEFT_PAREN)),
                    ')' => Some(Ok(TOKEN_RIGHT_PAREN)),
                    '{' => Some(Ok(TOKEN_LEFT_BRACE)),
                    '}' => Some(Ok(TOKEN_RIGHT_BRACE)),
                    ',' => Some(Ok(TOKEN_COMMA)),
                    '.' => Some(Ok(TOKEN_DOT)),
                    '+' => Some(Ok(TOKEN_PLUS)),
                    '-' => Some(Ok(TOKEN_MINUS)),
                    ';' => Some(Ok(TOKEN_SEMICOLON)),
                    '*' => Some(Ok(TOKEN_STAR)),
                    '"' => self.tokenize_string_literal(),
                    '/' => {
                        if self.peek() == Some('/') {
                            while self.peek().is_some_and(|c| c != '\n') {
                                self.advance();
                            }
                            continue;
                        } else {
                            Some(Ok(TOKEN_SLASH))
                        }
                    }
                    '>' => self.tokenize_op_or_opequal(TOKEN_GREATER, TOKEN_GREATER_EQ),
                    '<' => self.tokenize_op_or_opequal(TOKEN_LESS, TOKEN_LESS_EQ),
                    '=' => self.tokenize_op_or_opequal(TOKEN_EQ, TOKEN_EQ_EQ),
                    '!' => self.tokenize_op_or_opequal(TOKEN_BANG, TOKEN_BANG_EQ),
                    c if c.is_ascii_digit() => self.tokenize_number(),
                    c if c.is_alphanumeric() || c == '_' => self.tokenize_keyword_or_identifier(),
                    _ => {
                        return Some(Err(InvalidToken {
                            src: self.source.to_string(),
                            span: SourceSpan::from(self.offset().saturating_sub(1)),
                        }.into()));
                    }
                },
                None => None,
            };

            return t;
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("[line {}] Error: Unexpected character: {}", 
    .src[..=.span.offset()].lines().count(), 
    .src.chars().nth(.span.offset()).unwrap())]
#[diagnostic(code("65"))]
pub struct InvalidToken {
    #[source_code]
    src: String,
    #[label("here")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("[line {}] Error: Unterminated string.", 
    .src[..=.span.offset()].lines().count())]
#[diagnostic(code("65"))]
pub struct UnterminatedString {
    #[source_code]
    src: String,
    #[label("starting here")]
    span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(KeywordKind),
    Literal(LiteralKind),
    Number { raw: String, value: f64 },
    Identifier { value: String },
    String { value: String },
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(t) => write!(f, "{t}"),
            Token::Literal(t) => write!(f, "{t}"),
            Token::Number { raw, value } => {
                if *value == value.trunc() {
                    // tests require that integers are printed as N.0
                    write!(f, "NUMBER {raw} {value}.0")
                } else {
                    write!(f, "NUMBER {raw} {value}")
                }
            }
            Token::Identifier { value } => write!(f, "IDENTIFIER {value} null"),
            Token::String { value } => write!(f, "STRING \"{value}\" {value}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, IntoStaticStr)]
pub enum KeywordKind {
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

impl Display for KeywordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} null",
            <&KeywordKind as Into<&'static str>>::into(self).to_uppercase(),
            <&KeywordKind as Into<&'static str>>::into(self).to_lowercase()
        )
    }
}

#[derive(Debug, Clone, PartialEq, EnumMessage)]
pub enum LiteralKind {
    #[strum(message = "(", detailed_message = "LEFT_PAREN ( null")]
    LeftParen,
    #[strum(message = ")", detailed_message = "RIGHT_PAREN ) null")]
    RightParen,
    #[strum(message = "{", detailed_message = "LEFT_BRACE { null")]
    LeftBrace,
    #[strum(message = "}", detailed_message = "RIGHT_BRACE } null")]
    RightBrace,
    #[strum(message = ",", detailed_message = "COMMA , null")]
    Comma,
    #[strum(message = ".", detailed_message = "DOT . null")]
    Dot,
    #[strum(message = "-", detailed_message = "MINUS - null")]
    Minus,
    #[strum(message = "+", detailed_message = "PLUS + null")]
    Plus,
    #[strum(message = ";", detailed_message = "SEMICOLON ; null")]
    SemiColon,
    #[strum(message = "*", detailed_message = "STAR * null")]
    Star,
    #[strum(message = "=", detailed_message = "EQUAL = null")]
    Eq,
    #[strum(message = "==", detailed_message = "EQUAL_EQUAL == null")]
    EqEq,
    #[strum(message = "!", detailed_message = "BANG ! null")]
    Bang,
    #[strum(message = "!=", detailed_message = "BANG_EQUAL != null")]
    BangEq,
    #[strum(message = "<", detailed_message = "LESS < null")]
    Less,
    #[strum(message = "<=", detailed_message = "LESS_EQUAL <= null")]
    LessEq,
    #[strum(message = ">", detailed_message = "GREATER > null")]
    Greater,
    #[strum(message = ">=", detailed_message = "GREATER_EQUAL >= null")]
    GreaterEq,
    #[strum(message = "/", detailed_message = "SLASH / null")]
    Slash,
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_detailed_message().unwrap())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use itertools::assert_equal;

    #[test]
    fn punctuation() {
        let scanner = Lexer::new("/(){};,+-*===<=>=!=<>.!");

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![
            Token::Literal(LiteralKind::Slash),
            Token::Literal(LiteralKind::LeftParen),
            Token::Literal(LiteralKind::RightParen),
            Token::Literal(LiteralKind::LeftBrace),
            Token::Literal(LiteralKind::RightBrace),
            Token::Literal(LiteralKind::SemiColon),
            Token::Literal(LiteralKind::Comma),
            Token::Literal(LiteralKind::Plus),
            Token::Literal(LiteralKind::Minus),
            Token::Literal(LiteralKind::Star),
            Token::Literal(LiteralKind::EqEq),
            Token::Literal(LiteralKind::Eq),
            Token::Literal(LiteralKind::LessEq),
            Token::Literal(LiteralKind::GreaterEq),
            Token::Literal(LiteralKind::BangEq),
            Token::Literal(LiteralKind::Less),
            Token::Literal(LiteralKind::Greater),
            Token::Literal(LiteralKind::Dot),
            Token::Literal(LiteralKind::Bang),
        ];

        check(actual, expected);
    }

    #[test]
    fn keywords() {
        let keywords =
            "and class else false for fun if nil or print return super this true var while";
        let scanner = Lexer::new(keywords);

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![
            Token::Keyword(KeywordKind::And),
            Token::Keyword(KeywordKind::Class),
            Token::Keyword(KeywordKind::Else),
            Token::Keyword(KeywordKind::False),
            Token::Keyword(KeywordKind::For),
            Token::Keyword(KeywordKind::Fun),
            Token::Keyword(KeywordKind::If),
            Token::Keyword(KeywordKind::Nil),
            Token::Keyword(KeywordKind::Or),
            Token::Keyword(KeywordKind::Print),
            Token::Keyword(KeywordKind::Return),
            Token::Keyword(KeywordKind::Super),
            Token::Keyword(KeywordKind::This),
            Token::Keyword(KeywordKind::True),
            Token::Keyword(KeywordKind::Var),
            Token::Keyword(KeywordKind::While),
        ];

        check(actual, expected);
    }

    #[test]
    fn string_literals() {
        let input = "\"some string value\"";
        let scanner = Lexer::new(input);

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![Token::String {
            value: "some string value".to_string(),
        }];

        check(actual, expected);
    }

    #[test]
    fn string_literals_with_other_stuff() {
        let input = "var x = \"some string value\";";
        let scanner = Lexer::new(input);

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![
            Token::Keyword(KeywordKind::Var),
            Token::Identifier {
                value: "x".to_string(),
            },
            Token::Literal(LiteralKind::Eq),
            Token::String {
                value: "some string value".to_string(),
            },
            Token::Literal(LiteralKind::SemiColon),
        ];

        check(actual, expected);
    }

    #[test]
    fn addition_and_subtraction() {
        let input = "1 + 2 - 3";
        let scanner = Lexer::new(input);

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![
            number_token!("1".to_string(), 1.0),
            Token::Literal(LiteralKind::Plus),
            number_token!("2".to_string(), 2.0),
            Token::Literal(LiteralKind::Minus),
            number_token!("3".to_string(), 3.0),
        ];

        check(actual, expected);
    }

    #[test]
    fn numbers() {
        let input = "123 123.456 .456 123. 42.42";
        let scanner = Lexer::new(input);

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![
            number_token!("123".to_string(), 123.0),
            number_token!("123.456".to_string(), 123.456),
            Token::Literal(LiteralKind::Dot),
            number_token!("456".to_string(), 456.0),
            number_token!("123".to_string(), 123.0),
            Token::Literal(LiteralKind::Dot),
            number_token!("42.42".to_string(), 42.42),
        ];

        check(actual, expected);
    }

    #[test]
    fn identifiers() {
        let input = "(foo, bar, baz)";
        let scanner = Lexer::new(input);

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![
            Token::Literal(LiteralKind::LeftParen),
            Token::Identifier {
                value: "foo".to_string(),
            },
            Token::Literal(LiteralKind::Comma),
            Token::Identifier {
                value: "bar".to_string(),
            },
            Token::Literal(LiteralKind::Comma),
            Token::Identifier {
                value: "baz".to_string(),
            },
            Token::Literal(LiteralKind::RightParen),
        ];

        check(actual, expected);
    }

    fn check(actual: Vec<Token>, expected: Vec<Token>) {
        assert_eq!(actual.len(), expected.len());
        assert_equal(actual, expected);
    }
}
