use std::env;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = Scanner::default();

            if !file_contents.is_empty() {
                for c in file_contents.chars() {
                    writeln!(io::stderr(), "scanned '{}'", c).unwrap();
                    scanner.add_token(c.into());
                }
            }

            scanner.add_token(TOKEN_EOF);

            println!("{scanner}");

        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
        }
    }
}

#[derive(Debug)]
struct CharacterToken {
    display_name: &'static str,
    token: char,
}

#[derive(Debug)]
enum Tokens {
    LeftParen(CharacterToken),
    RightParen(CharacterToken),
    LeftBrace(CharacterToken),
    RightBrace(CharacterToken),
    NewLine(CharacterToken),
    Eof(&'static str),
}

const TOKEN_EOF: Tokens = Tokens::Eof("EOF");
const TOKEN_LEFT_PAREN: Tokens = Tokens::LeftParen(CharacterToken { display_name: "LEFT_PAREN", token: '(' });
const TOKEN_RIGHT_PAREN: Tokens = Tokens::RightParen(CharacterToken { display_name: "RIGHT_PAREN", token: ')' });
const TOKEN_LEFT_BRACE: Tokens = Tokens::LeftParen(CharacterToken { display_name: "LEFT_BRACE", token: '{' });
const TOKEN_RIGHT_BRACE: Tokens = Tokens::RightParen(CharacterToken { display_name: "RIGHT_BRACE", token: '}' });
const TOKEN_NL: Tokens = Tokens::RightParen(CharacterToken { display_name: "NL", token: '\n' });

#[derive(Debug, Default)]
struct Scanner {
    tokens: Vec<Tokens>,
}

impl Scanner {
    fn add_token(&mut self, t: Tokens) {
        self.tokens.push(t);
    }
}

impl Display for Scanner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.tokens.iter()
            .map(|t| format!("{} null", t.to_string()))
            .collect::<Vec<String>>()
            .join("\n"), f)
    }
}

impl From<char> for Tokens {
    fn from(value: char) -> Self {
        match value {
            '(' => TOKEN_LEFT_PAREN,
            ')' => TOKEN_RIGHT_PAREN,
            '{' => TOKEN_LEFT_BRACE,
            '}' => TOKEN_RIGHT_BRACE,
            '\n' => TOKEN_NL,
            _ => TOKEN_EOF,
        }
    }
}

impl Display for Tokens {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Tokens::LeftParen(t) => write!(f, "{} {}", t.display_name, t.token),
            Tokens::RightParen(t) => write!(f, "{} {}", t.display_name, t.token),
            Tokens::LeftBrace(t) => write!(f, "{} {}", t.display_name, t.token),
            Tokens::RightBrace(t) => write!(f, "{} {}", t.display_name, t.token),
            Tokens::NewLine(t) => write!(f, "{} {}", t.display_name, t.token),
            Tokens::Eof(s) => write!(f, "{} ", s),
        }
    }
}
