use std::env;
use std::fs;
use std::process;

mod scanner;
mod ast;
mod parser;
mod callable;
mod interpreter;

use crate::scanner::TokenType;
use crate::scanner::LiteralValue as ScannerLiteralValue;
use crate::ast::{Stmt, Program};

use crate::interpreter::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 3 {
        eprintln!("Usage: {} [tokenize|parse|run|evaluate] <filename>", args[0]);
        process::exit(64);
    }

    let (command, filename) = if args.len() == 3 {
        (args[1].as_str(), args[2].clone())
    } else {
        ("run", args[1].clone())
    };

    let source = match fs::read_to_string(&filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read '{}': {}", filename, e);
            process::exit(74);
        }
    };

    let (tokens, scan_errors) = scanner::scan_source(&source);
    for err in &scan_errors {
        eprintln!("{}", err);
    }
    if !scan_errors.is_empty() && (command == "parse" || command == "run" || command == "evaluate") {
        eprintln!("Exiting due to scanner errors.");
        process::exit(65);
    }

    match command {
        "tokenize" => {
            for tok in tokens {
                match &tok.literal {
                    Some(ScannerLiteralValue::Number(n)) =>
                        println!("{} {} {}", tok.token_type, tok.lexeme, n),
                    Some(ScannerLiteralValue::String(s)) =>
                        println!("{} {} {}", tok.token_type, tok.lexeme, s),
                    None =>
                        println!("{} {} null", tok.token_type, tok.lexeme),
                }
            }
            if !scan_errors.is_empty() {
                process::exit(65);
            }
        }

        "parse" => {
            if tokens.is_empty() || (tokens.len() == 1 && tokens[0].token_type == TokenType::EOF) {
                println!("No tokens to parse (or only EOF).");
                return;
            }
            let mut p = parser::Parser::new(tokens);
            match p.parse() {
                Ok(program) => {
                    if program.statements.len() == 1 {
                        if let Some(stmt) = program.statements.get(0) {
                            if let Stmt::ExprStmt(expr) = stmt {
                                println!("{}", expr);
                            } else {
                                println!("{}", stmt);
                            }
                        }
                    } else {
                        println!("{}", program);
                    }
                }
                Err(errors) => {
                    for e in errors {
                        eprintln!("{}", e);
                    }
                    process::exit(65);
                }
            }
        }

        "run" => {
            if tokens.is_empty() || (tokens.len() == 1 && tokens[0].token_type == TokenType::EOF) {
                return;
            }
            let mut p = parser::Parser::new(tokens.clone());
            let program = match p.parse() {
                Ok(prog) => prog,
                Err(errors) => {
                    for e in errors {
                        eprintln!("{}", e);
                    }
                    process::exit(65);
                }
            };
            let mut interp = Interpreter::new();
            if let Err(err) = interp.interpret(&program) {
                eprintln!("Runtime error: {}", err);
                process::exit(70);
            }
        }

        "evaluate" => {
            let mut p = parser::Parser::new(tokens);
            let expr = match p.parse_expression() {
                Ok(e) => e,
                Err(e) => {
                    eprintln!("{}", e);
                    process::exit(65);
                }
            };
            let program = Program::new(vec![Stmt::PrintStmt(Box::new(expr))]);
            let mut interp = Interpreter::new();
            if let Err(err) = interp.interpret(&program) {
                eprintln!("Runtime error: {}", err);
                process::exit(70);
            }
        }

        _ => {
            eprintln!("Unknown command: {}. Use 'tokenize', 'parse', 'run', or 'evaluate'.", command);
            process::exit(64);
        }
    }
}