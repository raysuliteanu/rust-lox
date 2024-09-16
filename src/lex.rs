use crate::token;
use crate::token::LiteralToken;
use crate::token::LiteralToken::{Bang, BangEq, Eq, EqEq, Greater, GreaterEq, Less, LessEq};
use anyhow::Error;
use itertools::Itertools;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read};
use std::path::PathBuf;
use token::LexToken;

#[derive(Debug)]
pub struct Scanner {
    pub tokens: Vec<LexToken>,
    source: String,
    // next char in 'source' to read
    next_char_idx: usize,
    // save it for the future ... might want to print filename in errors
    _filename: PathBuf,
    has_tokenization_err: bool,
    tokenization_only: bool,
}

impl Scanner {
    pub fn new(filename: &PathBuf, tokenization_only: bool) -> io::Result<Self> {
        let file = File::open(filename)?;
        let mut reader = BufReader::new(file);
        let mut source = String::new();
        reader.read_to_string(&mut source)?;

        Ok(Scanner {
            tokens: vec![],
            source,
            next_char_idx: 0,
            _filename: filename.clone(),
            has_tokenization_err: false,
            tokenization_only,
        })
    }

    #[cfg(test)]
    pub fn new_from_string(input: &str) -> Self {
        Scanner {
            tokens: vec![],
            source: String::from(input),
            next_char_idx: 0,
            _filename: PathBuf::from(""),
            has_tokenization_err: false,
            tokenization_only: true,
        }
    }

    pub fn tokenize(&mut self) -> anyhow::Result<()> {
        while let Some(token) = self.next() {
            match token {
                Ok(t) => match t {
                    // todo: how can we handle this better?
                    LexToken::Comment => {}
                    _ => {
                        if self.tokenization_only {
                            println!("{t}");
                        }
                        self.tokens.push(t);
                    }
                },
                Err(e) => {
                    if e.downcast_ref::<LexerError>().is_some() {
                        eprintln!("{e}");
                        self.has_tokenization_err = true;
                        continue;
                    }

                    return Err(e);
                }
            }
        }

        if self.tokenization_only {
            println!("EOF  null");
        }

        Ok(())
    }

    fn current_line(&self) -> usize {
        self.source[..self.next_char_idx].lines().count()
    }

    pub fn has_tokenization_err(&self) -> bool {
        self.has_tokenization_err
    }
}

impl Iterator for Scanner {
    type Item = Result<LexToken, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        enum StartOfToken {
            KeywordOrIdentifier,
            Number,
            StringLiteral,
            SlashOrComment,
            OpOrEqual(LiteralToken, LiteralToken),
        }

        loop {
            let c = self.source.chars().nth(self.next_char_idx)?;
            self.next_char_idx += 1;

            if c.is_whitespace() {
                continue;
            }

            let t = match c {
                '(' => return Some(Ok(LexToken::Literal(LiteralToken::LeftParen))),
                ')' => return Some(Ok(LexToken::Literal(LiteralToken::RightParen))),
                '{' => return Some(Ok(LexToken::Literal(LiteralToken::LeftBrace))),
                '}' => return Some(Ok(LexToken::Literal(LiteralToken::RightBrace))),
                ',' => return Some(Ok(LexToken::Literal(LiteralToken::Comma))),
                '.' => return Some(Ok(LexToken::Literal(LiteralToken::Dot))),
                '-' => return Some(Ok(LexToken::Literal(LiteralToken::Minus))),
                '+' => return Some(Ok(LexToken::Literal(LiteralToken::Plus))),
                ';' => return Some(Ok(LexToken::Literal(LiteralToken::SemiColon))),
                '*' => return Some(Ok(LexToken::Literal(LiteralToken::Star))),
                '"' => StartOfToken::StringLiteral,
                '/' => StartOfToken::SlashOrComment,
                '>' => StartOfToken::OpOrEqual(GreaterEq, Greater),
                '<' => StartOfToken::OpOrEqual(LessEq, Less),
                '=' => StartOfToken::OpOrEqual(EqEq, Eq),
                '!' => StartOfToken::OpOrEqual(BangEq, Bang),
                c if c.is_ascii_digit() => StartOfToken::Number,
                c if c.is_alphanumeric() || c == '_' => StartOfToken::KeywordOrIdentifier,
                _ => {
                    let e = LexerError::InvalidToken {
                        line: self.current_line(),
                        token: c,
                    };
                    return Some(Err(e.into()));
                }
            };

            // todo: some cleanup still could be done here w.r.t. almost duplicate code
            return match t {
                StartOfToken::Number => {
                    let start = self.next_char_idx - 1;

                    let offset = match self.source[start..]
                        .chars()
                        .find_position(|c| !c.is_ascii_digit())
                    {
                        Some((first_dot_off, '.')) => {
                            let mut count_digits_after_dot = 0;
                            for c in self.source[start + first_dot_off + 1..].chars() {
                                if c.is_ascii_digit() {
                                    count_digits_after_dot += 1;
                                } else {
                                    break;
                                }
                            }

                            if count_digits_after_dot > 0 {
                                start + first_dot_off + count_digits_after_dot + 1
                            // + 1 for the dot
                            } else {
                                // otherwise leave the dot to be tokenized next time through tokenize()
                                start + first_dot_off
                            }
                        }
                        Some((first_dot_off, _)) => start + first_dot_off,
                        None => {
                            // nothing but digits till the eof
                            self.source.len()
                        }
                    };

                    let value = self.source[start..offset].parse().unwrap();

                    self.next_char_idx = offset;

                    Some(Ok(LexToken::Number {
                        raw: String::from(&self.source[start..offset]),
                        value,
                    }))
                }
                StartOfToken::StringLiteral => {
                    let offset = self.next_char_idx;
                    if let Some(length) = self.source[offset..].find('"') {
                        self.next_char_idx += length + 1;
                        Some(Ok(LexToken::String {
                            value: String::from(&self.source[offset..offset + length]),
                        }))
                    } else {
                        let e = LexerError::UnterminatedString {
                            line: self.current_line(),
                        };
                        self.next_char_idx = self.source.len();
                        Some(Err(e.into()))
                    }
                }
                StartOfToken::SlashOrComment => {
                    if self
                        .source
                        .chars()
                        .nth(self.next_char_idx)
                        .is_some_and(|c| c == '/')
                    {
                        if let Some(pos) = self.source[self.next_char_idx..].find('\n') {
                            self.next_char_idx += pos + 1;
                            Some(Ok(LexToken::Comment))
                        } else {
                            self.next_char_idx = self.source.len();
                            None
                        }
                    } else {
                        Some(Ok(LexToken::Literal(LiteralToken::Slash)))
                    }
                }
                StartOfToken::OpOrEqual(l, r) => {
                    if self
                        .source
                        .chars()
                        .nth(self.next_char_idx)
                        .is_some_and(|c| c == '=')
                    {
                        self.next_char_idx += 1;
                        Some(Ok(LexToken::Literal(l)))
                    } else {
                        Some(Ok(LexToken::Literal(r)))
                    }
                }
                StartOfToken::KeywordOrIdentifier => {
                    let start = self.next_char_idx - 1;
                    // split_once will "remove" the space if found ... neither part contains the space
                    let word = match self.source[start..].split_once(|c: char| { !(c.is_alphanumeric() || c == '_') }) {
                        Some((word, _)) => word,
                        None => &self.source[start..],
                    };

                    let token = match word {
                        "and" => Some(Ok(LexToken::Keyword(token::KeywordToken::And))),
                        "class" => Some(Ok(LexToken::Keyword(token::KeywordToken::Class))),
                        "else" => Some(Ok(LexToken::Keyword(token::KeywordToken::Else))),
                        "false" => Some(Ok(LexToken::Keyword(token::KeywordToken::False))),
                        "for" => Some(Ok(LexToken::Keyword(token::KeywordToken::For))),
                        "fun" => Some(Ok(LexToken::Keyword(token::KeywordToken::Fun))),
                        "if" => Some(Ok(LexToken::Keyword(token::KeywordToken::If))),
                        "nil" => Some(Ok(LexToken::Keyword(token::KeywordToken::Nil))),
                        "or" => Some(Ok(LexToken::Keyword(token::KeywordToken::Or))),
                        "print" => Some(Ok(LexToken::Keyword(token::KeywordToken::Print))),
                        "return" => Some(Ok(LexToken::Keyword(token::KeywordToken::Return))),
                        "super" => Some(Ok(LexToken::Keyword(token::KeywordToken::Super))),
                        "this" => Some(Ok(LexToken::Keyword(token::KeywordToken::This))),
                        "true" => Some(Ok(LexToken::Keyword(token::KeywordToken::True))),
                        "var" => Some(Ok(LexToken::Keyword(token::KeywordToken::Var))),
                        "while" => Some(Ok(LexToken::Keyword(token::KeywordToken::While))),
                        _ => Some(Ok(LexToken::Identifier { value: String::from(&self.source[start..start + word.len()]) })),
                    };

                    self.next_char_idx += word.len() - 1; // -1 because we added one at the start

                    return token;
                }
            };
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum LexerError {
    #[error("[line {line}] Error: Unexpected character: {token}")]
    InvalidToken { line: usize, token: char },
    #[error("[line {line}] Error: Unterminated string.")]
    UnterminatedString { line: usize },
}

#[cfg(test)]
mod test {
    use itertools::assert_equal;

    use super::token::*;
    use super::Scanner;

    #[test]
    fn punctuation() {
        let mut scanner = Scanner::new_from_string("/(){};,+-*===<=>=!=<>.!");
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = scanner.tokens;
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
        let keywords = "and class else false for fun if nil or print return super this true var while";
        let mut scanner = Scanner::new_from_string(keywords);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = scanner.tokens;
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
        let mut scanner = Scanner::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = scanner.tokens;
        let expected = vec![LexToken::String {
            value: "some string value".to_string(),
        }];

        check(actual, expected);
    }

    #[test]
    fn string_literals_with_other_stuff() {
        let input = "var x = \"some string value\";";
        let mut scanner = Scanner::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = scanner.tokens;
        let expected = vec![
            LexToken::Keyword(KeywordToken::Var),
            LexToken::Identifier { value: "x".to_string() },
            LexToken::Literal(LiteralToken::Eq),
            LexToken::String {
                value: "some string value".to_string(),
            },
            LexToken::Literal(LiteralToken::SemiColon),
        ];

        check(actual, expected);
    }

    #[test]
    fn numbers() {
        let input = "123 123.456 .456 123. 42.42";
        let mut scanner = Scanner::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = scanner.tokens;
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
        ];

        check(actual, expected);
    }

    #[test]
    fn identifiers() {
        let input = "(foo, bar, baz)";
        let mut scanner = Scanner::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = scanner.tokens;
        let expected = vec![
            LexToken::Literal(LiteralToken::LeftParen),
            LexToken::Identifier { value: "foo".to_string(), },
            LexToken::Literal(LiteralToken::Comma),
            LexToken::Identifier { value: "bar".to_string(), },
            LexToken::Literal(LiteralToken::Comma),
            LexToken::Identifier { value: "baz".to_string(), },
            LexToken::Literal(LiteralToken::RightParen),
        ];

        check(actual, expected);
    }

    fn check(actual: Vec<LexToken>, expected: Vec<LexToken>) {
        assert_eq!(actual.len(), expected.len());
        assert_equal(actual, expected);
    }
}
