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
    InvalidAssignmentTarget { line: usize },
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
    last_consumed_token_line: usize,
    had_error: bool, 
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            last_consumed_token_line: 1, 
            had_error: false,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        let token = self.tokens.next();
        if let Some(ref t) = token {
            self.last_consumed_token_line = t.line;
        }
        token
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn is_at_end(&mut self) -> bool {
        match self.peek() {
            Some(token) => token.token_type == TokenType::EOF,
            None => true, // 
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
    
    fn consume(&mut self, expected_type: TokenType, error_message: &str) -> Result<Token, ParseError> {
        let peeked_token_info = match self.peek() {
            Some(token) => Some((token.token_type, token.line)), 
            None => None,
        };

        match peeked_token_info {
            Some((found_type, line)) if found_type == expected_type => {
                Ok(self.advance().unwrap()) 
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

    fn parse_declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.check(TokenType::VAR) {
          self.var_declaration()
        } else {
          self.statement()
        }
    }
    fn var_declaration(&mut self) -> Result<Stmt,ParseError> {
      self.advance();
      let name = self.consume(TokenType::IDENTIFIER, "Expect variable name.")?;
      let initializer = if self.check(TokenType::EQUAL) {
        self.advance();
        Some(self.expression()?)
      } else {
        None
      };
      self.consume(TokenType::SEMICOLON, "Expect ';' after variable declaration.")?;
      Ok(Stmt::VarDec(name, (initializer)))
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
        self.advance(); 
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
    
    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expr,ParseError> {
      let expr = self.parse_equality()?;
      if self.check(TokenType::EQUAL) {
        let equals = self.advance().unwrap();
        let value = self.assignment()?;
        if let Expr::Variable(name) = &*expr {
          return Ok(Expr::Assign(name.clone(), Box::new(value)));
        }
        return Err(ParseError::InvalidAssignmentTarget { line: name.line });
      }
      Ok(expr)
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> { 
        let expr = self.parse_equality()?;
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
    fn parse_addition(&mut self) -> Result<Expr, ParseError> {

        let mut expr = self.parse_multiplication()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::PLUS | TokenType::MINUS => {
                    let operator = self.advance().unwrap();
                    let right = self.parse_multiplication()?;
                    expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
                }
                _ => break,  
            }
        }

        Ok(expr)
    }

    fn parse_multiplication(&mut self) -> Result<Expr, ParseError> {

        let mut expr = self.primary()?;

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
        if let Some(token) = self.peek() { 
            let current_line = token.line;
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
                            let inner = self.expression()?; 
                            self.consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.")?;
                            Ok(Expr::Grouping(Box::new(inner)))
                        }
                        _ => unreachable!(), 
                    }
                }
                TokenType::BANG | TokenType::MINUS => { 
                    let operator_token = self.advance().unwrap(); 
                    let operand = self.primary()?; 
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

    fn check(&mut self, token_type: TokenType) -> bool {
        match self.peek() {
            Some(token) => token.token_type == token_type,
            None => false,
        }
    }
}
