use std::env;
use std::fs;
use std::fmt; // Add this import
use std::io::{self, Write};
use std::process; // For exiting gracefully

// Using standard Rust naming convention (PascalCase for enums/structs)
#[derive(Debug, Clone, PartialEq)] 
enum TokenType {
    LEFT_PAREN,  
    RIGHT_PAREN, 
    LEFT_BRACE,
    RIGHT_BRACE,
    STAR,
    DOT,
    COMMA,
    PLUS,
    MINUS,
    SEMICOLON,
    SLASH,
    EOF
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self) // Use Debug format for the type name string
    }
}

#[derive(Debug, Clone)] // Added Clone
struct Token {
    token_type: TokenType,
    lexeme: String,
    line: usize,
}

impl Token {
    fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

fn scan_source(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut line = 1; 
    for ch in source.chars() {
        match ch {
            '(' => {
                tokens.push(Token::new(
                    TokenType::LEFT_PAREN,
                    "(".to_string(), // The lexeme is just "("
                    line,
                ));
            }
            ')' => {
                tokens.push(Token::new(
                    TokenType::RIGHT_PAREN,
                    ")".to_string(), // The lexeme is just ")"
                    line,
                ));
            }
            '{' => {
              tokens.push(Token::new(
                TokenType::LEFT_BRACE,
                "{".to_string(),
                line
              ));
            }
            '}' => {
              tokens.push(Token::new(
                TokenType::RIGHT_BRACE,
                "}".to_string(),
                line,
              ));
            }
            ',' => {
              tokens.push(Token::new(
                TokenType::COMMA,
                ",".to_string(),
                line
              ));
            }
            '*' => {
              tokens.push(Token::new(
                TokenType::STAR,
                "*".to_string(),
                line
              ));
            }
            '+' => {
              tokens.push(Token::new(
                TokenType::PLUS,
                "+".to_string(),
                line,
              ));
            }
            '-' => {
              tokens.push(Token::new(TokenType::MINUS, "-".to_string(), line));
            }
            '.' => {
              tokens.push(Token::new(TokenType::DOT,".".to_string(),line));
            }
            ';' => {
              tokens.push(Token::new(TokenType::SEMICOLON,";".to_string(),line));
            }
            '/' => {
              tokens.push(Token::new(TokenType::SLASH,"/".to_string(),line));
            }
            '\n' => {
                line += 1;
            }
            ' ' | '\r' | '\t' => {
            }
            _ => {
                println!("[line {}] Ignoring unexpected character: {}", line, ch);
            }
        }
    }
    tokens.push(Token::new(
        TokenType::EOF,
        "".to_string(),
        line,
    ));

    tokens 
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        process::exit(64);
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = match fs::read_to_string(filename) {
                Ok(contents) => contents,
                Err(e) => {
                    eprintln!("Failed to read file '{}': {}", filename, e);
                    process::exit(74);
                }
            };

            // Call our scanner function!
            let tokens = scan_source(&file_contents);
            for token in tokens {
                 // Format: TYPE lexeme literal (using "null" for literal as required)
                println!("{} {} null", token.token_type, token.lexeme);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            process::exit(64);
        }
    }
}
