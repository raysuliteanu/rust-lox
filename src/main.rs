use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::{stderr, BufReader, Read, Write};
use std::process::ExitCode;
use std::{env, io, str};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return ExitCode::FAILURE;
    }

    let command = &args[1];
    if command != "tokenize" {
        eprintln!("Unknown command: {}", command);
        return ExitCode::FAILURE;
    }

    if let Ok(mut scanner) = Scanner::new(&args[2]) {
        let _ = scanner.scan();

        println!("{scanner}");

        if scanner.is_err() {
            return ExitCode::from(65);
        }
    }

    ExitCode::SUCCESS
}

#[derive(Debug)]
struct Scanner {
    tokens: Vec<TokenEntry>,
    cur_row: usize,
    cur_col: usize,
    file: File,
    source: Vec<char>,
}

impl Scanner {
    fn new(filename: &String) -> io::Result<Self> {
        let file = File::open(filename)?;
        Ok(Scanner {
            tokens: vec![],
            cur_row: 1,
            cur_col: 0,
            file,
            source: vec![],
        })
    }

    fn eof(&mut self) {
        self.tokens.push(TokenEntry {
            token: TOKEN_EOF,
            coord: Coordinate {
                row: self.cur_row + 1,
                col: 0,
            },
        });
    }

    fn is_err(&self) -> bool {
        self.tokens
            .iter()
            .any(|entry| matches!(entry.token, Token::Invalid(_)))
    }

    fn process_char(cur: char, reader: &mut CharReader) -> Option<Token> {
        match cur {
            '\n' => {
                // handle Windows style EOL '\n\r'
                if let Some('\r') = reader.peek() {
                    assert_eq!('\r', reader.read().unwrap());
                }
                Some(TOKEN_NL)
            }
            c if c.is_whitespace() => Some(TOKEN_WS),
            '/' => {
                // handle comment '//...\n'
                if let Some('/') = reader.peek() {
                    // eat 2nd '/'
                    assert_eq!('/', reader.read().unwrap());

                    loop {
                        let peek = reader.peek();
                        match peek {
                            Some('\n') => {
                                return Some(TOKEN_NL);
                            }
                            Some(_) => {
                                let _ = reader.read();
                            }
                            None => return None,
                        }
                    }
                } else {
                    Some(TOKEN_SLASH)
                }
            }
            '>' => {
                if let Some('=') = reader.peek() {
                    assert_eq!('=', reader.read().unwrap());
                    Some(TOKEN_GREATER_EQ)
                } else {
                    Some(TOKEN_GREATER)
                }
            }
            '<' => {
                if let Some('=') = reader.peek() {
                    assert_eq!('=', reader.read().unwrap());
                    Some(TOKEN_LESS_EQ)
                } else {
                    Some(TOKEN_LESS)
                }
            }
            '=' => {
                if let Some('=') = reader.peek() {
                    assert_eq!('=', reader.read().unwrap());
                    Some(TOKEN_EQ_EQ)
                } else {
                    Some(TOKEN_EQ)
                }
            }
            '!' => {
                if let Some('=') = reader.peek() {
                    assert_eq!('=', reader.read().unwrap());
                    Some(TOKEN_BANG_EQ)
                } else {
                    Some(TOKEN_BANG)
                }
            }
            '(' => Some(TOKEN_LEFT_PAREN),
            ')' => Some(TOKEN_RIGHT_PAREN),
            '{' => Some(TOKEN_LEFT_BRACE),
            '}' => Some(TOKEN_RIGHT_BRACE),
            ',' => Some(TOKEN_COMMA),
            '.' => Some(TOKEN_DOT),
            '-' => Some(TOKEN_DASH),
            '+' => Some(TOKEN_PLUS),
            ';' => Some(TOKEN_SEMI_COLON),
            '*' => Some(TOKEN_STAR),
            _ => Some(Token::Invalid(cur.to_string())),
        }
    }

    fn scan(&mut self) -> io::Result<&Self> {
        let mut reader = CharReader::new(&self.file);
        while let Some(c) = reader.read() {
            let token = Scanner::process_char(c, &mut reader);

            if let Some(token) = token {
                self.cur_col += 1;
                match token {
                    TOKEN_NL => {
                        self.cur_row += 1;
                        self.cur_col = 0;
                    }
                    Token::MultiChar(_) => {
                        // what if the str is more than 2 chars in len
                        // the token_len() function used here causes borrow issues :(
                        self.cur_col += 1;
                    }
                    _ => {}
                }

                match token {
                    TOKEN_WS => {
                        // do nothing
                    }
                    _ => {
                        self.tokens.push(TokenEntry {
                            token,
                            coord: Coordinate {
                                row: self.cur_row,
                                col: self.cur_col,
                            },
                        });
                    }
                }
            }
        }

        self.source.extend_from_slice(&reader.to_vec()[..]);

        self.eof();

        Ok(self)
    }
}

impl Display for Scanner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(
            &self
                .tokens
                .iter()
                .filter_map(|t| match &t.token {
                    &TOKEN_NL => None,
                    Token::Invalid(c) => {
                        stderr()
                            .write_fmt(format_args!(
                                "[line {}] Error: Unexpected character: {}\n",
                                t.coord.row, c
                            ))
                            .expect("failed to write to stderr");
                        None
                    }
                    _ => Some(format!("{} null", t.token)),
                })
                .collect::<Vec<String>>()
                .join("\n"),
            f,
        )
    }
}

#[derive(Debug, PartialEq)]
struct TokenEntry {
    token: Token,
    coord: Coordinate,
}

#[derive(Debug, PartialEq)]
struct Coordinate {
    row: usize,
    col: usize,
}

#[derive(Debug, PartialEq)]
struct CharacterToken {
    display_name: &'static str,
    token: char,
}

#[derive(Debug, PartialEq)]
struct MultiCharToken {
    display_name: &'static str,
    token: &'static str,
}

impl MultiCharToken {
    #[allow(dead_code)]
    fn token_len(&self) -> usize {
        self.token.len()
    }
}

#[derive(Debug, PartialEq)]
enum Token {
    Character(CharacterToken),
    MultiChar(MultiCharToken),
    Invalid(String),
    Eof(&'static str),
}

const TOKEN_EOF: Token = Token::Eof("EOF");
const TOKEN_LEFT_PAREN: Token = Token::Character(CharacterToken {
    display_name: "LEFT_PAREN",
    token: '(',
});
const TOKEN_RIGHT_PAREN: Token = Token::Character(CharacterToken {
    display_name: "RIGHT_PAREN",
    token: ')',
});
const TOKEN_LEFT_BRACE: Token = Token::Character(CharacterToken {
    display_name: "LEFT_BRACE",
    token: '{',
});
const TOKEN_RIGHT_BRACE: Token = Token::Character(CharacterToken {
    display_name: "RIGHT_BRACE",
    token: '}',
});
const TOKEN_NL: Token = Token::Character(CharacterToken {
    display_name: "NL",
    token: '\n',
});
const TOKEN_COMMA: Token = Token::Character(CharacterToken {
    display_name: "COMMA",
    token: ',',
});
const TOKEN_DOT: Token = Token::Character(CharacterToken {
    display_name: "DOT",
    token: '.',
});
const TOKEN_DASH: Token = Token::Character(CharacterToken {
    display_name: "MINUS",
    token: '-',
});
const TOKEN_PLUS: Token = Token::Character(CharacterToken {
    display_name: "PLUS",
    token: '+',
});
const TOKEN_SEMI_COLON: Token = Token::Character(CharacterToken {
    display_name: "SEMICOLON",
    token: ';',
});
const TOKEN_STAR: Token = Token::Character(CharacterToken {
    display_name: "STAR",
    token: '*',
});
const TOKEN_EQ: Token = Token::Character(CharacterToken {
    display_name: "EQUAL",
    token: '=',
});
const TOKEN_EQ_EQ: Token = Token::MultiChar(MultiCharToken {
    display_name: "EQUAL_EQUAL",
    token: "==",
});
const TOKEN_BANG: Token = Token::Character(CharacterToken {
    display_name: "BANG",
    token: '!',
});
const TOKEN_BANG_EQ: Token = Token::MultiChar(MultiCharToken {
    display_name: "BANG_EQUAL",
    token: "!=",
});
const TOKEN_LESS: Token = Token::Character(CharacterToken {
    display_name: "LESS",
    token: '<',
});
const TOKEN_LESS_EQ: Token = Token::MultiChar(MultiCharToken {
    display_name: "LESS_EQUAL",
    token: "<=",
});
const TOKEN_GREATER: Token = Token::Character(CharacterToken {
    display_name: "GREATER",
    token: '>',
});
const TOKEN_GREATER_EQ: Token = Token::MultiChar(MultiCharToken {
    display_name: "GREATER_EQUAL",
    token: ">=",
});
const TOKEN_SLASH: Token = Token::Character(CharacterToken {
    display_name: "SLASH",
    token: '/',
});
const TOKEN_WS: Token = Token::Character(CharacterToken {
    display_name: "WS",
    token: ' ',
});

impl From<char> for Token {
    fn from(value: char) -> Self {
        match value {
            '(' => TOKEN_LEFT_PAREN,
            ')' => TOKEN_RIGHT_PAREN,
            '{' => TOKEN_LEFT_BRACE,
            '}' => TOKEN_RIGHT_BRACE,
            '\n' => TOKEN_NL,
            ',' => TOKEN_COMMA,
            '.' => TOKEN_DOT,
            '-' => TOKEN_DASH,
            '+' => TOKEN_PLUS,
            ';' => TOKEN_SEMI_COLON,
            '*' => TOKEN_STAR,
            '=' => TOKEN_EQ,
            '!' => TOKEN_BANG,
            '<' => TOKEN_LESS,
            '>' => TOKEN_GREATER,
            '/' => TOKEN_SLASH,
            _ => Token::Invalid(value.to_string()),
        }
    }
}

impl From<&str> for Token {
    fn from(value: &str) -> Self {
        match value {
            "==" => TOKEN_EQ,
            "!=" => TOKEN_BANG_EQ,
            "<=" => TOKEN_LESS_EQ,
            ">=" => TOKEN_GREATER_EQ,
            _ => Token::Invalid(value.to_string()),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Character(t) => write!(f, "{} {}", t.display_name, t.token),
            Token::MultiChar(t) => write!(f, "{} {}", t.display_name, t.token),
            Token::Invalid(c) => write!(f, "{}", c), // shouldn't happen
            Token::Eof(s) => write!(f, "{} ", s),
        }
    }
}

struct CharReader<'a> {
    reader: BufReader<&'a File>,
    buffer: Vec<char>,
    offset: usize,
}

impl<'a> CharReader<'a> {
    fn new(file: &'a File) -> Self {
        CharReader {
            reader: BufReader::new(file),
            buffer: vec![],
            offset: 0,
        }
    }

    fn read(&mut self) -> Option<char> {
        if self.offset >= self.buffer.len() {
            self.expand_buffer();
        }

        if self.offset >= self.buffer.len() {
            None
        } else {
            let c = self.buffer[self.offset];
            self.offset += 1;
            Some(c)
        }
    }

    /// look at the current char without consuming it
    fn peek(&mut self) -> Option<char> {
        if self.offset >= self.buffer.len() {
            self.expand_buffer();
        }

        if self.offset >= self.buffer.len() {
            None
        } else {
            Some(self.buffer[self.offset])
        }
    }

    fn to_vec(&self) -> Vec<char> {
        self.buffer.to_vec()
    }

    fn expand_buffer(&mut self) {
        let mut buf = [0u8; 4096];
        if let Ok(n) = self.reader.read(&mut buf) {
            str::from_utf8(&buf[..n])
                .ok()
                .iter()
                .flat_map(|s| s.chars())
                .for_each(|c| self.buffer.push(c));
        }
    }
}
