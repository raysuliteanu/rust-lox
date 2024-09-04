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
    filename: PathBuf,
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
            filename: filename.clone(),
        })
    }

    pub fn tokenize(&mut self) -> anyhow::Result<()> {
        while let Some(token) = self.next() {
            match token {
                Ok(t) => {
                    println!("{t}");
                    self.tokens.push(t);
                }
                Err(e) => {
                    if let Some(LexerError::InvalidToken { line, token }) =
                        e.downcast_ref::<LexerError>()
                    {
                        eprintln!("[line: {}] {}", line, token);
                        continue;
                    }
                    return Err(e);
                }
            }
        }

        eprintln!("EOF  null");

        Ok(())
    }

    fn current_line(&self) -> usize {
        self.source[..self.next_char_idx].lines().count()
    }
}

impl Iterator for Scanner {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        enum StartOfToken {
            StringLiteral,
            SlashOrComment,
            OpOrEqual(LiteralToken, LiteralToken),
        }

        loop {
            let (i, c) = self.source.char_indices().nth(self.next_char_idx)?;

            self.next_char_idx = i + 1;

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
                '-' => return Some(Ok(Token::Literal(LiteralToken::Dash))),
                '+' => return Some(Ok(Token::Literal(LiteralToken::Plus))),
                ';' => return Some(Ok(Token::Literal(LiteralToken::SemiColon))),
                '*' => return Some(Ok(Token::Literal(LiteralToken::Star))),
                '"' => StartOfToken::StringLiteral,
                '/' => StartOfToken::SlashOrComment,
                '>' => StartOfToken::OpOrEqual(GreaterEq, Greater),
                '<' => StartOfToken::OpOrEqual(LessEq, Less),
                '=' => StartOfToken::OpOrEqual(EqEq, Eq),
                '!' => StartOfToken::OpOrEqual(BangEq, Bang),
                c if c.is_whitespace() => continue,
                _ => {
                    let e = LexerError::InvalidToken {
                        line: self.current_line(),
                        token: c,
                    };
                    return Some(Err(e.into()));
                }
            };

            match t {
                StartOfToken::StringLiteral => todo!("string literal"),
                StartOfToken::SlashOrComment => {
                    let (i, c) = self.source.char_indices().nth(self.next_char_idx)?;
                    self.next_char_idx = i + 1;

                    if c == '/' {
                        todo!("eat comment")
                    } else {
                        return Some(Ok(Token::Literal(LiteralToken::Slash)));
                    };
                }
                StartOfToken::OpOrEqual(l, r) => {
                    let (i, c) = self.source.char_indices().nth(self.next_char_idx)?;

                    return if c == '=' {
                        self.next_char_idx = i + 1;
                        Some(Ok(Token::Literal(l)))
                    } else {
                        Some(Ok(Token::Literal(r)))
                    };
                }
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum LexerError {
    #[error("Invalid character: {token}")]
    InvalidToken { line: usize, token: char },
}
