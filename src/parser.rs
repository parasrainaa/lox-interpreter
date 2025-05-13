use crate::scanner::{Token, TokenType, LiteralValue as ScannerLiteralValue};
use crate::ast::{Expr, AstLiteralValue};
use std::iter::Peekable;
use std::vec;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken { expected: String, found: String, line: usize },
    UnexpectedEndOfInput { line: usize },
    ExpectedPrimaryExpression { found: String, line: usize },
    InvalidNumberLiteral { value: String, line: usize },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found, line } => 
                write!(f, "[line {}] Error: Expected {} but found {}.", line, expected, found),
            ParseError::UnexpectedEndOfInput { line } => 
                write!(f, "[line {}] Error: Unexpected end of input.", line),
            ParseError::ExpectedPrimaryExpression { found, line } => 
                write!(f, "[line {}] Error: Expected primary expression but found {}.", line, found),
            ParseError::InvalidNumberLiteral { value, line } => 
                write!(f, "[line {}] Error: Invalid number literal '{}'.", line, value),
        }
    }
}

impl std::error::Error for ParseError {}

pub struct Parser {
    tokens: Peekable<vec::IntoIter<Token>>,
    // Keep track of the last consumed token's line for error reporting
    last_consumed_token_line: usize, 
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            last_consumed_token_line: 1, // Default to 1, will be updated upon first advance
        }
    }

    // Consumes the next token and updates the last consumed token's line.
    fn advance(&mut self) -> Option<Token> {
        let token = self.tokens.next();
        if let Some(ref t) = token {
            self.last_consumed_token_line = t.line;
        }
        token
    }

    // Looks at the next token without consuming it.
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        // Your main.rs already checks for completely empty or EOF-only token streams.
        // So, we expect at least one non-EOF token if this is called.
        let expr = self.primary()?;

        // After parsing a primary expression, for this stage, we expect no more tokens other than EOF.
        if let Some(token) = self.peek() {
            if token.token_type != TokenType::EOF {
                return Err(ParseError::UnexpectedToken {
                    expected: "EOF".to_string(),
                    found: format!("{:?}", token.token_type),
                    line: token.line,
                });
            }
        }
        // If peek() is None here, it implies the primary() consumed the last token and it wasn't EOF.
        // This shouldn't happen if the scanner always adds EOF and primary() doesn't consume it.
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.advance() {
            let current_line = token.line; // Line of the token being processed
            match token.token_type {
                TokenType::FALSE => Ok(Expr::Literal(AstLiteralValue::Boolean(false))),
                TokenType::TRUE => Ok(Expr::Literal(AstLiteralValue::Boolean(true))),
                TokenType::NIL => Ok(Expr::Literal(AstLiteralValue::Nil)),
                TokenType::NUMBER => {
                    match token.literal {
                        Some(ScannerLiteralValue::Number(s)) => {
                            s.parse::<f64>()
                                .map(|n| Expr::Literal(AstLiteralValue::Number(n)))
                                .map_err(|_e| ParseError::InvalidNumberLiteral { value: s.clone(), line: current_line })
                        }
                        _ => Err(ParseError::ExpectedPrimaryExpression{
                            found: "Number token without number literal".to_string(), 
                            line: current_line 
                        }),
                    }
                },
                TokenType::STRING => {
                    match token.literal {
                        Some(ScannerLiteralValue::String(s)) => {
                            Ok(Expr::Literal(AstLiteralValue::StringValue(s)))
                        }
                        _ => Err(ParseError::ExpectedPrimaryExpression{
                            found: "String token without string literal".to_string(), 
                            line: current_line 
                        }),
                    }
                },
                TokenType::EOF => {
                    // This means an expression was expected, but we found EOF instead.
                    Err(ParseError::UnexpectedEndOfInput { line: current_line })
                }
                _ => Err(ParseError::ExpectedPrimaryExpression {
                    found: format!("{:?}", token.token_type),
                    line: current_line,
                }),
            }
        } else {
            // This means advance() returned None, meaning no tokens were left when primary() was called.
            // Given main.rs checks, this path should ideally not be hit if called from parse_expression directly.
            // If it is, use the last known line number.
            Err(ParseError::UnexpectedEndOfInput { line: self.last_consumed_token_line })
        }
    }
}