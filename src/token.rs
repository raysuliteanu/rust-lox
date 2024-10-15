use miette::{Diagnostic, SourceSpan};
use std::{fmt::Display, str::Chars};
use thiserror::Error;

pub struct Lexer<'le> {
    _source_file: String,
    source: &'le str,
    offset: usize,
    _chars: Chars<'le>,
}

impl<'le> Lexer<'le> {
    pub fn new(source_file: String, source: &'le str) -> Self {
        Self {
            _source_file: source_file,
            source,
            offset: 0,
            _chars: source.chars(),
        }
    }

    #[cfg(test)]
    pub fn new_from_string(source: &'le str) -> Self {
        Self::new("test".to_string(), source)
    }

    fn offset(&self) -> usize {
        self.offset
    }

    fn peek(&mut self) -> Option<char> {
        // self.chars.nth(self.offset)
        self.source.chars().nth(self.offset)
    }

    fn advance(&mut self) -> Option<char> {
        // let next = self.chars.nth(self.offset)?;
        let next = self.source.chars().nth(self.offset)?;
        self.offset += 1;

        Some(next)
    }

    fn tokenize_keyword_or_identifier(&mut self) -> Option<miette::Result<Token>> {
        let start = self.offset - 1;
        // split_once will "remove" the space if found ... neither part contains the space
        let word =
            match self.source[start..].split_once(|c: char| !(c.is_alphanumeric() || c == '_')) {
                Some((word, _)) => word,
                None => &self.source[start..],
            };

        let token = match word {
            "and" => Token::Keyword(KeywordKind::And),
            "class" => Token::Keyword(KeywordKind::Class),
            "else" => Token::Keyword(KeywordKind::Else),
            "false" => Token::Keyword(KeywordKind::False),
            "for" => Token::Keyword(KeywordKind::For),
            "fun" => Token::Keyword(KeywordKind::Fun),
            "if" => Token::Keyword(KeywordKind::If),
            "nil" => Token::Keyword(KeywordKind::Nil),
            "or" => Token::Keyword(KeywordKind::Or),
            "print" => Token::Keyword(KeywordKind::Print),
            "return" => Token::Keyword(KeywordKind::Return),
            "super" => Token::Keyword(KeywordKind::Super),
            "this" => Token::Keyword(KeywordKind::This),
            "true" => Token::Keyword(KeywordKind::True),
            "var" => Token::Keyword(KeywordKind::Var),
            "while" => Token::Keyword(KeywordKind::While),
            _ => Token::Identifier {
                value: String::from(&self.source[start..start + word.len()]),
            },
        };

        self.offset += word.len() - 1; // -1 because we added one at the start

        Some(Ok(token))
    }

    fn tokenize_number(&mut self) -> Option<miette::Result<Token>> {
        let start = self.offset - 1;

        let non_digit_idx = self.source[start..]
            .find(|c| !matches!(c, '.' | '0'..='9'))
            .unwrap_or(self.source.len() - start);

        let mut num_literal = &self.source[start..start + non_digit_idx];
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

        Some(Ok(Token::Number {
            raw: String::from(num_literal),
            value,
        }))
    }

    fn tokenize_op_or_opequal(
        &mut self,
        this: LiteralKind,
        that: LiteralKind,
    ) -> Option<miette::Result<Token>> {
        self.peek()
            .is_some_and(|c| c == '=')
            .then(|| {
                assert_eq!(self.advance(), Some('=')); // eat the '='
                Ok(Token::Literal(that))
            })
            .or(Some(Ok(Token::Literal(this))))
    }

    fn tokenize_string_literal(&mut self) -> Option<miette::Result<Token>> {
        let offset = self.offset();
        if let Some(length) = self.source[offset..].find('"') {
            self.offset += length + 1;
            Some(Ok(Token::String {
                value: String::from(&self.source[offset..offset + length]),
            }))
        } else {
            let e = UnterminatedString {
                src: self.source.to_string(),
                span: SourceSpan::from(self.offset),
            };
            self.offset = self.source.len();
            Some(Err(e.into()))
        }
    }
}

impl<'le> Iterator for Lexer<'le> {
    type Item = Result<Token, miette::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let cur_char = self.advance();
            let t = match cur_char {
                Some(c) => match c {
                    c if c.is_whitespace() => continue,
                    '(' => Some(Ok(Token::Literal(LiteralKind::LeftParen))),
                    ')' => Some(Ok(Token::Literal(LiteralKind::RightParen))),
                    '{' => Some(Ok(Token::Literal(LiteralKind::LeftBrace))),
                    '}' => Some(Ok(Token::Literal(LiteralKind::RightBrace))),
                    ',' => Some(Ok(Token::Literal(LiteralKind::Comma))),
                    '.' => Some(Ok(Token::Literal(LiteralKind::Dot))),
                    '+' => Some(Ok(Token::Literal(LiteralKind::Plus))),
                    '-' => Some(Ok(Token::Literal(LiteralKind::Minus))),
                    ';' => Some(Ok(Token::Literal(LiteralKind::SemiColon))),
                    '*' => Some(Ok(Token::Literal(LiteralKind::Star))),
                    '"' => self.tokenize_string_literal(),
                    '/' => {
                        if self.peek() == Some('/') {
                            while self.peek().is_some_and(|c| c != '\n') {
                                self.advance();
                            }
                            continue;
                        } else {
                            Some(Ok(Token::Literal(LiteralKind::Slash)))
                        }
                    }
                    '>' => {
                        self.tokenize_op_or_opequal(LiteralKind::Greater, LiteralKind::GreaterEq)
                    }
                    '<' => self.tokenize_op_or_opequal(LiteralKind::Less, LiteralKind::LessEq),
                    '=' => self.tokenize_op_or_opequal(LiteralKind::Eq, LiteralKind::EqEq),
                    '!' => self.tokenize_op_or_opequal(LiteralKind::Bang, LiteralKind::BangEq),
                    c if c.is_ascii_digit() => self.tokenize_number(),
                    c if c.is_alphanumeric() || c == '_' => self.tokenize_keyword_or_identifier(),
                    _ => {
                        let error = InvalidToken {
                            src: self.source.to_string(),
                            span: SourceSpan::from(self.offset()),
                        };

                        return Some(Err(error.into()));
                    }
                },
                None => None,
            };

            return t;
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("[line {}] Error: Unexpected character: {}", .src[..=.span.offset() - 1].lines().count(), .src.chars().nth(.span.offset() - 1).unwrap()
)]
pub struct InvalidToken {
    #[source_code]
    src: String,
    #[label("here")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("[line {}] Error: Unterminated string", .src[..=.span.offset() - 1].lines().count())]
pub struct UnterminatedString {
    #[source_code]
    src: String,
    #[label("here")]
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

#[derive(Debug, Clone, PartialEq)]
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
        match self {
            KeywordKind::And => write!(f, "AND and null"),
            KeywordKind::Class => write!(f, "CLASS class null"),
            KeywordKind::Else => write!(f, "ELSE else null"),
            KeywordKind::False => write!(f, "FALSE false null"),
            KeywordKind::For => write!(f, "FOR for null"),
            KeywordKind::Fun => write!(f, "FUN fun null"),
            KeywordKind::If => write!(f, "IF if null"),
            KeywordKind::Nil => write!(f, "NIL nil null"),
            KeywordKind::Or => write!(f, "OR or null"),
            KeywordKind::Print => write!(f, "PRINT print null"),
            KeywordKind::Return => write!(f, "RETURN return null"),
            KeywordKind::Super => write!(f, "SUPER super null"),
            KeywordKind::This => write!(f, "THIS this null"),
            KeywordKind::True => write!(f, "TRUE true null"),
            KeywordKind::Var => write!(f, "VAR var null"),
            KeywordKind::While => write!(f, "WHILE while null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
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

impl Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralKind::LeftParen => write!(f, "LEFT_PAREN ( null"),
            LiteralKind::RightParen => write!(f, "RIGHT_PAREN ) null"),
            LiteralKind::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            LiteralKind::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            LiteralKind::Comma => write!(f, "COMMA , null"),
            LiteralKind::Dot => write!(f, "DOT . null"),
            LiteralKind::Minus => write!(f, "MINUS - null"),
            LiteralKind::Plus => write!(f, "PLUS + null"),
            LiteralKind::SemiColon => write!(f, "SEMICOLON ; null"),
            LiteralKind::Star => write!(f, "STAR * null"),
            LiteralKind::Eq => write!(f, "EQUAL = null"),
            LiteralKind::EqEq => write!(f, "EQUAL_EQUAL == null"),
            LiteralKind::Bang => write!(f, "BANG ! null"),
            LiteralKind::BangEq => write!(f, "BANG_EQUAL != null"),
            LiteralKind::Less => write!(f, "LESS < null"),
            LiteralKind::LessEq => write!(f, "LESS_EQUAL <= null"),
            LiteralKind::Greater => write!(f, "GREATER > null"),
            LiteralKind::GreaterEq => write!(f, "GREATER_EQUAL >= null"),
            LiteralKind::Slash => write!(f, "SLASH / null"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use itertools::assert_equal;

    #[test]
    fn punctuation() {
        let scanner = Lexer::new_from_string("/(){};,+-*===<=>=!=<>.!");

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
        let scanner = Lexer::new_from_string(keywords);

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
        let scanner = Lexer::new_from_string(input);

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
        let scanner = Lexer::new_from_string(input);

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
        let scanner = Lexer::new_from_string(input);

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![
            Token::Number {
                raw: "1".to_string(),
                value: 1.0,
            },
            Token::Literal(LiteralKind::Plus),
            Token::Number {
                raw: "2".to_string(),
                value: 2.0,
            },
            Token::Literal(LiteralKind::Minus),
            Token::Number {
                raw: "3".to_string(),
                value: 3.0,
            },
        ];

        check(actual, expected);
    }

    #[test]
    fn numbers() {
        let input = "123 123.456 .456 123. 42.42";
        let scanner = Lexer::new_from_string(input);

        let actual = scanner
            .into_iter()
            .map(|x| x.unwrap_or_else(|e| panic!("{:?}", e)))
            .collect::<Vec<_>>();

        let expected = vec![
            Token::Number {
                raw: "123".to_string(),
                value: 123.0,
            },
            Token::Number {
                raw: "123.456".to_string(),
                value: 123.456,
            },
            Token::Literal(LiteralKind::Dot),
            Token::Number {
                raw: "456".to_string(),
                value: 456.0,
            },
            Token::Number {
                raw: "123".to_string(),
                value: 123.0,
            },
            Token::Literal(LiteralKind::Dot),
            Token::Number {
                raw: "42.42".to_string(),
                value: 42.42,
            },
        ];

        check(actual, expected);
    }

    #[test]
    fn identifiers() {
        let input = "(foo, bar, baz)";
        let scanner = Lexer::new_from_string(input);

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
