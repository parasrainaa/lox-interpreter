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

    /// Entry‐point: start at the lowest‐precedence level
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_equality()?;          // ← NEW: delegate to addition level
        // … then your EOF check exactly as you have it now …
        if let Some(token) = self.peek() {
            if token.token_type != TokenType::EOF {
                return Err(ParseError::UnexpectedToken {
                    expected: "EOF".into(),
                    found: format!("{:?}", token.token_type),
                    line: token.line,
                });
            }
        }
        Ok(expr)
    }
    fn parse_equality(&mut self) -> Result<Expr,ParseError> {
       let mut expr: Expr =  self.parse_comparison()?;
       while let Some(token) = self.peek() {
        match token.token_type {
            TokenType::BANG_EQUAL | TokenType::EQUAL_EQUAL => {
              let operator = self.advance().unwrap();
              let right =  self.parse_comparison()?;
              expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            }
        _ => break,
        }
       }
       Ok(expr)
    }
    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.parse_addition()?;
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::GREATER | TokenType::GREATER_EQUAL | TokenType::LESS |
                TokenType::LESS_EQUAL => {
                    let operator = self.advance().unwrap();
                    let right_expr  = self.parse_addition()?;
                    expr = Expr::Binary(Box::new(expr),operator,Box::new(right_expr));
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    /// addition → multiplication ( ( "+" | "-" ) multiplication )*
    fn parse_addition(&mut self) -> Result<Expr, ParseError> {
        // 1. Parse the "left" side by delegating to the next‐higher level
        let mut expr = self.parse_multiplication()?;

        // 2. As long as we see + or −, consume it and parse another multiplication()
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::PLUS | TokenType::MINUS => {
                    let operator = self.advance().unwrap();      // we know peek was Some
                    let right = self.parse_multiplication()?;
                    // combine into a new Binary node, and loop again
                    expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
                }
                _ => break,  // no more +/− at this level
            }
        }

        Ok(expr)
    }

    /// multiplication → primary ( ( "*" | "/" ) primary )*
    fn parse_multiplication(&mut self) -> Result<Expr, ParseError> {
        // 1. Parse the "left" side by calling primary() (which already handles unary)
        let mut expr = self.primary()?;

        // 2. As long as we see * or /, consume it and parse another primary()
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::STAR | TokenType::SLASH => {
                    let operator = self.advance().unwrap();
                    let right = self.primary()?;
                    expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.advance() {
            let current_line = token.line;
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
                TokenType::LEFT_PAREN => {
                    // Parse the full expression inside the parens (including binary ops)
                    let inner = self.parse_equality()?;

                    // Then we must see a RIGHT_PAREN
                    match self.advance() {
                        Some(t) if t.token_type == TokenType::RIGHT_PAREN => {
                            Ok(Expr::Grouping(Box::new(inner)))
                        }
                        Some(t) => {
                            // Found some other token instead of ')'
                            Err(ParseError::UnexpectedToken {
                                expected: "RIGHT_PAREN".to_string(),
                                found: format!("{:?}", t.token_type),
                                line: t.line,
                            })
                        }
                        None => {
                            // Ran out of tokens entirely
                            Err(ParseError::UnexpectedEndOfInput {
                                line: self.last_consumed_token_line,
                            })
                        }
                    }
                },
                TokenType::BANG | TokenType::MINUS => {
                    let operator = token.clone();
                    let operand = self.primary()?;
                    Ok(Expr::Unary(operator, Box::new(operand)))
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
