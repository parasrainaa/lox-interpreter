use std::env;
use std::fmt; // Add this import
use std::fs;
use std::io::{self, Write};
use std::process; // For exiting gracefully
use phf::Map;
use phf_macros::phf_map;

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
    STRING,
    NUMBER,
    EOF,
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    IDENTIFIER,
}

// Static map for reserved keywords
static KEYWORDS: Map<&'static str, TokenType> = phf_map! {
    "and" => TokenType::AND,
    "class" => TokenType::CLASS,
    "else" => TokenType::ELSE,
    "false" => TokenType::FALSE,
    "for" => TokenType::FOR,
    "fun" => TokenType::FUN,
    "if" => TokenType::IF,
    "nil" => TokenType::NIL,
    "or" => TokenType::OR,
    "print" => TokenType::PRINT,
    "return" => TokenType::RETURN,
    "super" => TokenType::SUPER,
    "this" => TokenType::THIS,
    "true" => TokenType::TRUE,
    "var" => TokenType::VAR,
    "while" => TokenType::WHILE,
};

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self) // Use Debug format for the type name string
    }
}

// Define possible literal values
#[derive(Debug, Clone, PartialEq)]
enum LiteralValue {
    Number(String),
    String(String),
}

#[derive(Debug, Clone)] // Added Clone
struct Token {
    token_type: TokenType,
    lexeme: String,
    line: usize,
    literal: Option<LiteralValue>,
}

impl Token {
    fn new(token_type: TokenType, lexeme: String, line: usize, literal: Option<LiteralValue>) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            literal,
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
                tokens.push(Token::new(TokenType::LEFT_PAREN, "(".to_string(), line, None));
            }
            ')' => {
                tokens.push(Token::new(TokenType::RIGHT_PAREN, ")".to_string(), line, None));
            }
            '{' => {
                tokens.push(Token::new(TokenType::LEFT_BRACE, "{".to_string(), line, None));
            }
            '}' => {
                tokens.push(Token::new(TokenType::RIGHT_BRACE, "}".to_string(), line, None));
            }
            ',' => {
                tokens.push(Token::new(TokenType::COMMA, ",".to_string(), line, None));
            }
            '*' => {
                tokens.push(Token::new(TokenType::STAR, "*".to_string(), line, None));
            }
            '+' => {
                tokens.push(Token::new(TokenType::PLUS, "+".to_string(), line, None));
            }
            '-' => {
                tokens.push(Token::new(TokenType::MINUS, "-".to_string(), line, None));
            }
            '.' => {
                tokens.push(Token::new(TokenType::DOT, ".".to_string(), line, None));
            }
            ';' => {
                tokens.push(Token::new(TokenType::SEMICOLON, ";".to_string(), line, None));
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
                    tokens.push(Token::new(TokenType::SLASH, "/".to_string(), line, None));
                }
            }
            '=' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::new(TokenType::EQUAL_EQUAL, "==".to_string(), line, None));
                } else {
                    tokens.push(Token::new(TokenType::EQUAL, "=".to_string(), line, None));
                }
            }
            '!' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::new(TokenType::BANG_EQUAL, "!=".to_string(), line, None));
                } else {
                    tokens.push(Token::new(TokenType::BANG, "!".to_string(), line, None));
                }
            }
            '<' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::new(TokenType::LESS_EQUAL, "<=".to_string(), line, None));
                } else {
                    tokens.push(Token::new(TokenType::LESS, "<".to_string(), line, None));
                }
            }
            '>' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::new(TokenType::GREATER_EQUAL, ">=".to_string(), line, None));
                } else {
                    tokens.push(Token::new(TokenType::GREATER, ">".to_string(), line, None));
                }
            }
            '"' => {
                let mut lexeme = String::from("\"");
                let mut content = String::new();
                let start_line = line;
                let mut terminated = false;

                while let Some(next_ch) = chars.next() {
                    lexeme.push(next_ch);
                    if next_ch == '"' {
                        terminated = true;
                        break;
                    }
                    if next_ch == '\n' {
                        line += 1;
                    }
                    content.push(next_ch);
                }

                if terminated {
                    tokens.push(Token::new(TokenType::STRING, lexeme, start_line, Some(LiteralValue::String(content))));
                } else {
                    errors.push(format!("[line {}] Error: Unterminated string.", start_line));
                }
            }
            '\n' => {
                line += 1;
            }
            ' ' | '\r' | '\t' => {
                // Ignore whitespace
            }
            // Handle Identifiers and Keywords
            _ if ch.is_ascii_alphabetic() || ch == '_' => {
                let mut identifier = String::new();
                identifier.push(ch);

                // Consume the rest of the identifier
                while let Some(&peek_ch) = chars.peek() {
                    if peek_ch.is_ascii_alphanumeric() || peek_ch == '_' {
                        identifier.push(chars.next().unwrap());
                    } else {
                        break; 
                    }
                }

                // Check if it's a keyword or an identifier
                let token_type = KEYWORDS.get(&identifier)
                                      .cloned()
                                      .unwrap_or(TokenType::IDENTIFIER);
                
                tokens.push(Token::new(token_type, identifier, line, None));
            }
            // Handle Numbers
            _ if ch.is_ascii_digit() => {
                 let mut number_str = String::new();
                 number_str.push(ch);
 
                 while let Some(&peek_ch) = chars.peek() {
                     if peek_ch.is_ascii_digit() {
                         number_str.push(chars.next().unwrap());
                     } else {
                         break;
                     }
                 }

                 let mut has_decimal = false;
                 if let Some(&'.') = chars.peek() {
                     let mut next_chars = chars.clone();
                     if let Some('.') = next_chars.next() {
                         if let Some(digit_after_dot) = next_chars.peek() {
                             if digit_after_dot.is_ascii_digit() {
                                 number_str.push(chars.next().unwrap());
                                 has_decimal = true;
                                 while let Some(&peek_ch) = chars.peek() {
                                     if peek_ch.is_ascii_digit() {
                                         number_str.push(chars.next().unwrap());
                                     } else {
                                         break;
                                     }
                                 }
                             }
                         }
                     }
                 }

                 // Format the literal value as required
                 let literal = if has_decimal {
                     if let Some(dot_pos) = number_str.find('.') {
                         let (int_part, frac_part) = number_str.split_at(dot_pos);
                         let mut frac = &frac_part[1..]; // skip the '.'
                         frac = frac.trim_end_matches('0');
                         if frac.is_empty() {
                             format!("{}.0", int_part)
                         } else {
                             format!("{}.{}", int_part, frac)
                         }
                     } else {
                         format!("{}.0", number_str)
                     }
                 } else {
                     format!("{}.0", number_str)
                 };

                 tokens.push(Token::new(
                     TokenType::NUMBER,
                     number_str,
                     line,
                     Some(LiteralValue::Number(literal)),
                 ));
             }
            // Handle unexpected characters
            _ => {
                let error_message = format!("[line {}] Error: Unexpected character: {}", line, ch);
                errors.push(error_message);
            }
        }
    }
    tokens.push(Token::new(TokenType::EOF, "".to_string(), line, None));

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

            let (tokens, errors) = scan_source(&file_contents);

            for token in tokens {
                match token.literal {
                    Some(LiteralValue::Number(ref n)) => {
                        println!("{} {} {}", token.token_type, token.lexeme, n);
                    }
                    Some(LiteralValue::String(ref s)) => {
                        println!("{} {} {}", token.token_type, token.lexeme, s);
                    }
                    None => {
                        println!("{} {} null", token.token_type, token.lexeme);
                    }
                }
            }

            for error in &errors {
                eprintln!("{}", error);
            }

            if !errors.is_empty() {
                process::exit(65);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            process::exit(64);
        }
    }
}
