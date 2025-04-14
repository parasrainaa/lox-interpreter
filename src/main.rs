use std::env;
use std::fmt; // Add this import
use std::fs;
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
    EQUAL,
    EQUAL_EQUAL,
    BANG,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    EOF,
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

// Returns tokens and a vector of error messages.
fn scan_source(source: &str) -> (Vec<Token>, Vec<String>) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new(); // Collect error messages
    let mut line = 1;
    let mut chars = source.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '(' => {
                tokens.push(Token::new(TokenType::LEFT_PAREN, "(".to_string(), line));
            }
            ')' => {
                tokens.push(Token::new(TokenType::RIGHT_PAREN, ")".to_string(), line));
            }
            '{' => {
                tokens.push(Token::new(TokenType::LEFT_BRACE, "{".to_string(), line));
            }
            '}' => {
                tokens.push(Token::new(TokenType::RIGHT_BRACE, "}".to_string(), line));
            }
            ',' => {
                tokens.push(Token::new(TokenType::COMMA, ",".to_string(), line));
            }
            '*' => {
                tokens.push(Token::new(TokenType::STAR, "*".to_string(), line));
            }
            '+' => {
                tokens.push(Token::new(TokenType::PLUS, "+".to_string(), line));
            }
            '-' => {
                tokens.push(Token::new(TokenType::MINUS, "-".to_string(), line));
            }
            '.' => {
                tokens.push(Token::new(TokenType::DOT, ".".to_string(), line));
            }
            ';' => {
                tokens.push(Token::new(TokenType::SEMICOLON, ";".to_string(), line));
            }
            '/' => {
                if chars.peek() == Some(&'/') {
                    chars.next();
                    while let Some(next_ch) = chars.next() {
                        if next_ch == '\n' {
                            line += 1;
                            break;
                        }
                    }
                } else {
                    tokens.push(Token::new(TokenType::SLASH, "/".to_string(), line));
                }
            }
            '=' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::new(TokenType::EQUAL_EQUAL, "==".to_string(), line));
                } else {
                    tokens.push(Token::new(TokenType::EQUAL, "=".to_string(), line));
                }
            }
            '!' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::new(TokenType::BANG_EQUAL, "!=".to_string(), line));
                } else {
                    tokens.push(Token::new(TokenType::BANG, "!".to_string(), line));
                }
            }
            '<' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::new(TokenType::LESS_EQUAL, "<=".to_string(), line));
                } else {
                    tokens.push(Token::new(TokenType::LESS, "<".to_string(), line));
                }
            }
            '>' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::new(TokenType::GREATER_EQUAL, ">=".to_string(), line));
                } else {
                    tokens.push(Token::new(TokenType::GREATER, ">".to_string(), line));
                }
            }
            '\n' => {
                line += 1;
            }
            ' ' | '\r' | '\t' => {
                // Ignore whitespace
            }
            _ => {
                // Collect error message for unexpected characters
                let error_message = format!("[line {}] Error: Unexpected character: {}", line, ch);
                errors.push(error_message);
            }
        }
    }
    tokens.push(Token::new(TokenType::EOF, "".to_string(), line));

    (tokens, errors)
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
            let (tokens, errors) = scan_source(&file_contents);

            // Print valid tokens to stdout
            for token in tokens {
                // Format: TYPE lexeme literal (using "null" for literal as required)
                println!("{} {} null", token.token_type, token.lexeme);
            }

            // Print errors to stderr
            for error in &errors {
                // Iterate over borrowed errors
                eprintln!("{}", error);
            }

            // Exit with code 65 if errors occurred
            if !errors.is_empty() {
                process::exit(65); // Exit code 65: data format error
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            process::exit(64);
        }
    }
}
