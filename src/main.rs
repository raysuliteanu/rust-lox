use std::env;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return ExitCode::FAILURE;
    }

    let command = &args[1];
    let filename = &args[2];

    let mut scanner = Scanner::new();

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            if !file_contents.is_empty() {
                for c in file_contents.chars() {
                    scanner.add_char(c);
                }
            }

            scanner.eof();

            println!("{scanner}");
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }

    if scanner.is_err() {
        return ExitCode::from(65);
    }

    ExitCode::SUCCESS
}

#[derive(Debug)]
struct Scanner {
    tokens: Vec<TokenEntry>,
    cur_row: usize,
    cur_col: usize,
}

impl Scanner {
    fn new() -> Self {
        Scanner {
            tokens: vec![],
            cur_row: 1,
            cur_col: 0,
        }
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

    fn add_char(&mut self, c: char) {
        let token = c.into();

        if c == '\n' {
            self.cur_row += 1;
            self.cur_col = 0;
        } else {
            self.cur_col += 1;
        }

        let coord = Coordinate {
            row: self.cur_row,
            col: self.cur_col,
        };

        self.tokens.push(TokenEntry { 
            token, 
            coord, 
        });
    }

    fn is_err(&self) -> bool {
        self.tokens
            .iter()
            .any(|entry| matches!(entry.token, Token::Invalid(_)))
    }
}

impl Display for Scanner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(
            &self
                .tokens
                .iter()
                .filter_map(|t| match t.token {
                    TOKEN_NL => None,
                    Token::Invalid(c) => {
                        eprintln!("[line {}] Error: Unexpected character: {}", t.coord.row, c);
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
enum Token {
    Character(CharacterToken),
    Invalid(char),
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
            _ => Token::Invalid(value),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Character(t) => write!(f, "{} {}", t.display_name, t.token),
            Token::Invalid(c) => write!(f, "{}", c), // shouldn't happen
            Token::Eof(s) => write!(f, "{} ", s),
        }
    }
}
