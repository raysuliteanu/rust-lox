use crate::token;
use crate::token::LiteralToken;
use crate::token::LiteralToken::{Bang, BangEq, Eq, EqEq, Greater, GreaterEq, Less, LessEq};
use anyhow::Error;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read};
use std::path::PathBuf;
use token::Token;

#[derive(Debug)]
pub struct Scanner {
    tokens: Vec<Token>,
    source: String,
    // next char in 'source' to read
    next_char_idx: usize,
    // save it for the future ... might want to print filename in errors
    _filename: PathBuf,
    has_tokenization_err: bool,
}

impl Scanner {
    pub fn new(filename: &PathBuf) -> io::Result<Self> {
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
        }
    }

    pub fn tokenize(&mut self) -> anyhow::Result<()> {
        while let Some(token) = self.next() {
            match token {
                Ok(t) => match t {
                    // todo: how can we handle this better?
                    Token::Comment => {}
                    _ => {
                        println!("{t}");
                        self.tokens.push(t);
                    }
                },
                Err(e) => {
                    if let Some(LexerError::InvalidToken { line, token }) =
                        e.downcast_ref::<LexerError>()
                    {
                        eprintln!("[line {}] Error: Unexpected character: {}", line, token);
                        self.has_tokenization_err = true;
                        continue;
                    }

                    return Err(e);
                }
            }
        }

        println!("EOF  null");

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
    type Item = Result<Token, Error>;

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
                '(' => return Some(Ok(Token::Literal(LiteralToken::LeftParen))),
                ')' => return Some(Ok(Token::Literal(LiteralToken::RightParen))),
                '{' => return Some(Ok(Token::Literal(LiteralToken::LeftBrace))),
                '}' => return Some(Ok(Token::Literal(LiteralToken::RightBrace))),
                ',' => return Some(Ok(Token::Literal(LiteralToken::Comma))),
                '.' => return Some(Ok(Token::Literal(LiteralToken::Dot))),
                '-' => return Some(Ok(Token::Literal(LiteralToken::Minus))),
                '+' => return Some(Ok(Token::Literal(LiteralToken::Plus))),
                ';' => return Some(Ok(Token::Literal(LiteralToken::SemiColon))),
                '*' => return Some(Ok(Token::Literal(LiteralToken::Star))),
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

            // todo: some cleanup stll could be done here w.r.t. almost duplicate code
            return match t {
                StartOfToken::StringLiteral => {
                    if let Some(length) = self.source[self.next_char_idx..].find('"') {
                        let offset = self.next_char_idx - 1;
                        self.next_char_idx += length + 1;
                        Some(Ok(Token::String { offset, length }))
                    } else {
                        todo!("handle error")
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
                            Some(Ok(Token::Comment))
                        } else {
                            self.next_char_idx = self.source.len();
                            None
                        }
                    } else {
                        Some(Ok(Token::Literal(LiteralToken::Slash)))
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
                        Some(Ok(Token::Literal(l)))
                    } else {
                        Some(Ok(Token::Literal(r)))
                    }
                }
                StartOfToken::KeywordOrIdentifier => {
                    let start = self.next_char_idx - 1;
                    // split_once will "remove" the space if found ... neither part contains the space
                    let word = match self.source[start..].split_once(' ') {
                        Some((word, _)) => word,
                        None => &self.source[start..],
                    };

                    let token = match word {
                        "and" => Some(Ok(Token::Keyword(token::KeywordToken::And))),
                        "class" => Some(Ok(Token::Keyword(token::KeywordToken::Class))),
                        "else" => Some(Ok(Token::Keyword(token::KeywordToken::Else))),
                        "false" => Some(Ok(Token::Keyword(token::KeywordToken::False))),
                        "for" => Some(Ok(Token::Keyword(token::KeywordToken::For))),
                        "fun" => Some(Ok(Token::Keyword(token::KeywordToken::Fun))),
                        "if" => Some(Ok(Token::Keyword(token::KeywordToken::If))),
                        "nil" => Some(Ok(Token::Keyword(token::KeywordToken::Nil))),
                        "or" => Some(Ok(Token::Keyword(token::KeywordToken::Or))),
                        "return" => Some(Ok(Token::Keyword(token::KeywordToken::Return))),
                        "super" => Some(Ok(Token::Keyword(token::KeywordToken::Super))),
                        "this" => Some(Ok(Token::Keyword(token::KeywordToken::This))),
                        "true" => Some(Ok(Token::Keyword(token::KeywordToken::True))),
                        "var" => Some(Ok(Token::Keyword(token::KeywordToken::Var))),
                        "while" => Some(Ok(Token::Keyword(token::KeywordToken::While))),
                        _ => Some(Ok(Token::Identifier {
                            offset: start,
                            length: word.len(),
                        })),
                    };

                    // +1 for the space; if there wasn't one that's ok
                    self.next_char_idx += word.len();

                    return token;
                }
                StartOfToken::Number => {
                    let _start = self.next_char_idx - 1;
                    let _off = self.source[self.next_char_idx..].find('.');
                    todo!("parse numbers")
                }
            };
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum LexerError {
    #[error("Unexpected character: {token}")]
    InvalidToken { line: usize, token: char },
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
            Token::Literal(LiteralToken::Slash),
            Token::Literal(LiteralToken::LeftParen),
            Token::Literal(LiteralToken::RightParen),
            Token::Literal(LiteralToken::LeftBrace),
            Token::Literal(LiteralToken::RightBrace),
            Token::Literal(LiteralToken::SemiColon),
            Token::Literal(LiteralToken::Comma),
            Token::Literal(LiteralToken::Plus),
            Token::Literal(LiteralToken::Minus),
            Token::Literal(LiteralToken::Star),
            Token::Literal(LiteralToken::EqEq),
            Token::Literal(LiteralToken::Eq),
            Token::Literal(LiteralToken::LessEq),
            Token::Literal(LiteralToken::GreaterEq),
            Token::Literal(LiteralToken::BangEq),
            Token::Literal(LiteralToken::Less),
            Token::Literal(LiteralToken::Greater),
            Token::Literal(LiteralToken::Dot),
            Token::Literal(LiteralToken::Bang),
        ];

        check(actual, expected);
    }

    #[test]
    fn keywords() {
        let keywords = "and class else false for fun if nil or return super this true var while";
        let mut scanner = Scanner::new_from_string(keywords);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = scanner.tokens;
        let expected = vec![
            Token::Keyword(KeywordToken::And),
            Token::Keyword(KeywordToken::Class),
            Token::Keyword(KeywordToken::Else),
            Token::Keyword(KeywordToken::False),
            Token::Keyword(KeywordToken::For),
            Token::Keyword(KeywordToken::Fun),
            Token::Keyword(KeywordToken::If),
            Token::Keyword(KeywordToken::Nil),
            Token::Keyword(KeywordToken::Or),
            Token::Keyword(KeywordToken::Return),
            Token::Keyword(KeywordToken::Super),
            Token::Keyword(KeywordToken::This),
            Token::Keyword(KeywordToken::True),
            Token::Keyword(KeywordToken::Var),
            Token::Keyword(KeywordToken::While),
        ];

        check(actual, expected);
    }

    fn check(actual: Vec<Token>, expected: Vec<Token>) {
        assert_eq!(actual.len(), expected.len());
        assert_equal(actual, expected);
    }

    #[test]
    fn string_literals() {
        let input = "\"some string value\"";
        let mut scanner = Scanner::new_from_string(input);
        let result = scanner.tokenize();

        assert!(result.is_ok());

        let actual = scanner.tokens;
        let expected = vec![Token::String {
            offset: 0,
            length: input.len() - 2, // for the '\' escapes
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
            Token::Keyword(KeywordToken::Var),
            Token::Identifier {
                offset: 4,
                length: 1,
            },
            Token::Literal(LiteralToken::Eq),
            Token::String {
                offset: 8,
                length: 17,
            },
            Token::Literal(LiteralToken::SemiColon),
        ];

        check(actual, expected);
    }
}
