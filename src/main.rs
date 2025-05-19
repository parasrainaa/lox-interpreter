use std::env;
use std::fs;
use std::process; // For exiting gracefully

// Declare the scanner module (looks for src/scanner.rs or src/scanner/mod.rs)
mod scanner;
mod ast;
mod parser;
mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 3 { // Allowing `lox <filename>` or `lox` (for eventual REPL)
        eprintln!("Usage: {} [filename]", args[0]);
        eprintln!("Usage: {} <filename>", args[0]);
        process::exit(64);
    }

    let (command_or_filename, filename_option) = if args.len() == 2 {
        (args[1].clone(), None) // Treat as `lox <filename>` implicitly meaning "run"
    } else { // args.len() == 3
        (args[1].clone(), Some(args[2].clone()))
    };

    let filename = if filename_option.is_some() {
        filename_option.unwrap()
    } else {
        command_or_filename.clone() // This is the filename if command was implicit
    };

    let command = if args.len() == 3 { command_or_filename } else { "run".to_string() };

    let file_contents = match fs::read_to_string(&filename) {
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

    if !scanner_errors.is_empty() && (command == "parse" || command == "evaluate" || command == "run") {
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
            match parser.parse() {
                Ok(program_ast) => {
                    println!("Parsed {} statements successfully.", program_ast.statements.len());
                }
                Err(parse_errors) => {
                    for error in parse_errors {
                        eprintln!("{}", error);
                    }
                    process::exit(65);
                }
            }
        }
        "evaluate" | "run" => {
            if tokens.is_empty() || (tokens.len() == 1 && tokens[0].token_type == scanner::TokenType::EOF) {
                return;
            }
            let mut parser = parser::Parser::new(tokens);
            match parser.parse() {
                Ok(program_ast) => {
                    let mut interpreter = interpreter::Interpreter::new();
                    match interpreter.interpret(&program_ast) {
                        Ok(()) => { /* Execution successful */ }
                        Err(runtime_error) => {
                            eprintln!("Runtime error: {}", runtime_error);
                            process::exit(70);
                        }
                    }
                }
                Err(parse_errors) => {
                    for error in parse_errors {
                        eprintln!("{}", error);
                    }
                    process::exit(65);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}. Use 'tokenize', 'parse', or 'evaluate'/'run'.", command);
            process::exit(64);
        }
    }
}
