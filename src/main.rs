use std::env;
use std::fs;
use std::io::{self, Write};
use std::process; // For exiting gracefully

// Declare the scanner module (looks for src/scanner.rs or src/scanner/mod.rs)
mod scanner;
mod ast;
mod parser;
mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} <tokenize|parse|evaluate> <filename>", args[0]);
        process::exit(64);
    }

    let command = &args[1];
    let filename = &args[2];

            let file_contents = match fs::read_to_string(filename) {
                Ok(contents) => contents,
                Err(e) => {
                    eprintln!("Failed to read file '{}': {}", filename, e);
                    process::exit(74);
                }
            };

    let (tokens, scanner_errors) = scanner::scan_source(&file_contents);

    for error in &scanner_errors {
        eprintln!("{}", error);
    }

    if !scanner_errors.is_empty() && (command == "parse" || command == "evaluate") {
        eprintln!("Exiting due to scanner errors before parsing/evaluation.");
        process::exit(65);
    }


    match command.as_str() {
        "tokenize" => {
            for token in tokens {
                match &token.literal { 
                    Some(scanner::LiteralValue::Number(n)) => {
                        println!("{} {} {}", token.token_type, token.lexeme, n);
                    }
                    Some(scanner::LiteralValue::String(s)) => {
                        println!("{} {} {}", token.token_type, token.lexeme, s);
                    }
                    None => {
                        println!("{} {} null", token.token_type, token.lexeme);
                    }
                }
            }
            if !scanner_errors.is_empty() {
                process::exit(65);
            }
        }
        "parse" => {
            if tokens.is_empty() || (tokens.len() == 1 && tokens[0].token_type == scanner::TokenType::EOF) {
                println!("No tokens to parse (or only EOF).");
                return;
            }

            let mut parser = parser::Parser::new(tokens);
            match parser.parse_expression() {
                Ok(expr_ast) => {
                    // Print the parsed AST
                    println!("{}", expr_ast);
                }
                Err(parse_error) => {
                    eprintln!("{}", parse_error);
                    process::exit(65);
                }
            }
        }
        "evaluate" => {
            // Evaluate: parse then interpret
            if tokens.is_empty() || (tokens.len() == 1 && tokens[0].token_type == scanner::TokenType::EOF) {
                // Nothing to evaluate
                return;
            }
            let mut parser = parser::Parser::new(tokens);
            match parser.parse_expression() {
                Ok(expr_ast) => {
                    match interpreter::evaluate(&expr_ast) {
                        Ok(value) => println!("{}", value),
                        Err(err) => {
                            eprintln!("{:?}", err);
                            process::exit(70);
                        }
                    }
                }
                Err(parse_error) => {
                    eprintln!("{}", parse_error);
                    process::exit(65);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}. Use 'tokenize', 'parse', or 'evaluate'.", command);
            process::exit(64);
        }
    }
}
