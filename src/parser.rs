use crate::scanner::{Token, TokenType, LiteralValue as ScannerLiteralValue};
use crate::ast::{Expr, AstLiteralValue, Stmt, Program};
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
    // A flag to track if an error occurred to help with synchronization.
    // This is a simple way to avoid cascading errors. A more robust
    // panic mode recovery would involve more complex logic.
    had_error: bool, 
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            last_consumed_token_line: 1, // Default to 1, will be updated upon first advance
            had_error: false,
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

    // Checks if the next token is EOF
    fn is_at_end(&mut self) -> bool {
        match self.peek() {
            Some(token) => token.token_type == TokenType::EOF,
            None => true, // No more tokens means we are at the end
        }
    }
    fn synchronize(&mut self) {
      while let Some(tok) = self.advance() {
          if tok.token_type == TokenType::SEMICOLON {
              return;
          }
          match self.peek().map(|t| &t.token_type) {
              Some(TokenType::CLASS) | Some(TokenType::FUN) | Some(TokenType::VAR)
              | Some(TokenType::FOR)   | Some(TokenType::IF)  | Some(TokenType::WHILE)
              | Some(TokenType::PRINT) | Some(TokenType::RETURN) => return,
              _ => {}
          }
      }
  }
    
    // Consumes a token if it matches the expected type.
    // Returns the consumed token or an error.
    fn consume(&mut self, expected_type: TokenType, error_message: &str) -> Result<Token, ParseError> {
        let peeked_token_info = match self.peek() {
            Some(token) => Some((token.token_type, token.line)), // Extract info needed
            None => None,
        };

        match peeked_token_info {
            Some((found_type, line)) if found_type == expected_type => {
                Ok(self.advance().unwrap()) // Safe to unwrap, we just confirmed it's there and matches
            }
            Some((found_type, line)) => {
                self.had_error = true;
                Err(ParseError::UnexpectedToken {
                    expected: format!("{:?}", expected_type),
                    found: format!("{:?}", found_type),
                    line,
                })
            }
            None => {
                self.had_error = true;
                Err(ParseError::UnexpectedEndOfInput {
                    line: self.last_consumed_token_line,
                })
            }
        }
    }
    // Entry point for declarations (currently only statements).
    fn parse_declaration(&mut self) -> Result<Stmt, ParseError> {
        // In this interpreter, declarations are the same as statements.
        self.statement()
    }

    pub fn parse(&mut self) -> Result<Program, Vec<ParseError>> {
      let mut errors = Vec::new();
      let mut statements = Vec::new();
      while let Some(tok) = self.peek() {
          if tok.token_type == TokenType::EOF {
              self.advance();
              break;
          }
          match self.parse_declaration() {
              Ok(stmt) => statements.push(stmt),
              Err(err) => {
                  errors.push(err);
                  self.synchronize();
              }
          }
      }
      if errors.is_empty() {
          Ok(Program::new(statements))
      } else {
          Err(errors)
      }
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check(TokenType::PRINT) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // Consume PRINT token
        if self.check(TokenType::SEMICOLON) {
          return Err(ParseError::UnexpectedToken {
              expected: "expression".to_string(),
              found: "semicolon".to_string(),
              line: self.peek().unwrap().line,
          });
      }
        let value = self.expression()?;
        if !self.is_at_end() {
            self.consume(TokenType::SEMICOLON, "Expect ';' after value.")?;
        }
        Ok(Stmt::PrintStmt(Box::new(value)))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        if !self.is_at_end() {
            self.consume(TokenType::SEMICOLON, "Expect ';' after expression.")?;
        }
        Ok(Stmt::ExprStmt(Box::new(expr)))
    }
    
    // Renaming parse_expression to expression for clarity as it's now a part of statement parsing
    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_equality()
    }

    /// Entry‐point: start at the lowest‐precedence level
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> { // This method might be used by REPL or tests
        let expr = self.parse_equality()?;
        // For a single expression, we might still want to ensure no trailing tokens other than EOF
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
            // Helper to check current token type without consuming
            // Useful for statement parsing logic
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
        if let Some(token) = self.peek() { // Changed advance() to peek() to check before consuming
            let current_line = token.line;
            // We consume the token only if it's part of a valid primary expression
            match token.token_type {
                TokenType::FALSE | TokenType::TRUE | TokenType::NIL | 
                TokenType::NUMBER | TokenType::STRING | TokenType::LEFT_PAREN => {
                    let consumed_token = self.advance().unwrap(); // Now consume
                    match consumed_token.token_type {
                        TokenType::FALSE => Ok(Expr::Literal(AstLiteralValue::Boolean(false))),
                        TokenType::TRUE => Ok(Expr::Literal(AstLiteralValue::Boolean(true))),
                        TokenType::NIL => Ok(Expr::Literal(AstLiteralValue::Nil)),
                        TokenType::NUMBER => {
                            match consumed_token.literal {
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
                            match consumed_token.literal {
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
                            let inner = self.expression()?; // Changed from parse_equality to expression
                            self.consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.")?;
                            Ok(Expr::Grouping(Box::new(inner)))
                        }
                        _ => unreachable!(), // Should not happen due to outer match
                    }
                }
                TokenType::BANG | TokenType::MINUS => { // Unary operators
                    let operator_token = self.advance().unwrap(); // Consume operator
                    let operand = self.primary()?; // Parse the operand (which is a primary expression)
                    Ok(Expr::Unary(operator_token, Box::new(operand)))
                }
                TokenType::EOF => {
                    Err(ParseError::UnexpectedEndOfInput { line: current_line })
                }
                _ => Err(ParseError::ExpectedPrimaryExpression {
                    found: format!("{:?}", token.token_type),
                    line: current_line,
                }),
            }
        } else {
            Err(ParseError::UnexpectedEndOfInput { line: self.last_consumed_token_line })
        }
    }

    // Helper to check current token type without consuming
    fn check(&mut self, token_type: TokenType) -> bool {
        match self.peek() {
            Some(token) => token.token_type == token_type,
            None => false,
        }
    }
}
