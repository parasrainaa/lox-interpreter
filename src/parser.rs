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
            ParseError::InvalidAssignmentTarget { line } =>
                write!(f, "[line {}] Error: Invalid assignment target.", line),
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
        if self.check(TokenType::FUN) {
          return self.function_declaration();
        }
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
        Some(Box::new(self.expression()?))
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

    // --------------------
    // STATEMENT PARSING
    // --------------------
    // statement → forStmt | ifStmt | whileStmt | block | printStmt | expressionStmt ;
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check(TokenType::FOR) {
            return self.for_statement();
        }
        if self.check(TokenType::IF) {
            return self.if_statement();
        }
        if self.check(TokenType::WHILE) {
            return self.while_statement();
        }
        if self.check(TokenType::RETURN) {
            return self.return_statement();
        }
        if self.check(TokenType::LEFT_BRACE) {
            // Block statement
            self.advance(); // consume '{'
            let stmts = self.block()?;
            Ok(Stmt::Block(stmts))
        } else if self.check(TokenType::PRINT) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    // block → "{" declaration* "}" ;
    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RIGHT_BRACE) && !self.is_at_end() {
            statements.push(self.parse_declaration()?);
        }

        // Consume the closing '}'
        self.consume(TokenType::RIGHT_BRACE, "Expect '}' after block.")?;
        Ok(statements)
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
    // assignment → IDENTIFIER "=" assignment | logic_or ;
    fn assignment(&mut self) -> Result<Expr,ParseError> {
      let expr = self.parse_or()?;
      if self.check(TokenType::EQUAL) {
        let equals_token = self.advance().unwrap();
        let value = self.assignment()?;
        if let Expr::Variable(name) = &expr {
          return Ok(Expr::Assign(name.clone(), Box::new(value)));
        }
        return Err(ParseError::InvalidAssignmentTarget { line: equals_token.line });
      }
      Ok(expr)
    }

    // logic_or → logic_and ( "or" logic_and )* ;
    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_and()?;
        while self.check(TokenType::OR) {
            let operator = self.advance().unwrap();
            let right = self.parse_and()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // logic_and → equality ( "and" equality )* ;
    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        while self.check(TokenType::AND) {
            let operator = self.advance().unwrap();
            let right = self.parse_equality()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
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
        let mut expr = self.parse_unary()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::STAR | TokenType::SLASH => {
                    let operator = self.advance().unwrap();
                    let right = self.parse_unary()?;
                    expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.check(TokenType::BANG) || self.check(TokenType::MINUS) {
            let operator = self.advance().unwrap();
            let right = self.parse_unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.check(TokenType::LEFT_PAREN) {
                self.advance();
                let mut args = Vec::new();
                if !self.check(TokenType::RIGHT_PAREN) {
                    loop {
                        args.push(self.expression()?);
                        if !self.check(TokenType::COMMA) {
                            break;
                        }
                        self.advance(); // consume comma
                    }
                }
                let closing = self.consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments.")?;
                expr = Expr::Call(Box::new(expr), closing, args);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.peek() { 
            let current_line = token.line;
            match token.token_type {
                TokenType::FALSE | TokenType::TRUE | TokenType::NIL | 
                TokenType::NUMBER | TokenType::STRING | TokenType::LEFT_PAREN | TokenType::IDENTIFIER => {
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
                        TokenType::IDENTIFIER => {
                            Ok(Expr::Variable(consumed_token))
                        }
                        _ => unreachable!(), 
                    }
                }
                // BANG and MINUS are handled in parse_unary with proper precedence.
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

    // forStmt → "for" "(" ( varDecl | expressionStmt | ";" )
    //           expression? ";"
    //           expression? ")" statement ;
    // We desugar it into a combination of initializer + while loop.
    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        // consume 'for'
        self.advance();

        self.consume(TokenType::LEFT_PAREN, "Expect '(' after 'for'.")?;

        // 1. initializer
        let initializer: Option<Stmt> = if self.check(TokenType::SEMICOLON) {
            self.advance();
            None
        } else if self.check(TokenType::VAR) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        // 2. condition
        let condition: Option<Expr> = if !self.check(TokenType::SEMICOLON) {
            Some(self.expression()?)
        } else { None };
        self.consume(TokenType::SEMICOLON, "Expect ';' after loop condition.")?;

        // 3. increment
        let increment: Option<Expr> = if !self.check(TokenType::RIGHT_PAREN) {
            Some(self.expression()?)
        } else { None };
        self.consume(TokenType::RIGHT_PAREN, "Expect ')' after for clauses.")?;

        // 4. Body
        let mut body = self.statement()?;

        // Append increment at end of body if present
        if let Some(inc_expr) = increment {
            body = Stmt::Block(vec![body, Stmt::ExprStmt(Box::new(inc_expr))]);
        }

        // Condition defaults to true
        let condition_expr = if let Some(cond) = condition {
            cond
        } else {
            Expr::Literal(AstLiteralValue::Boolean(true))
        };

        // Wrap body in while loop
        body = Stmt::While(Box::new(condition_expr), Box::new(body));

        // Prepend initializer if present
        if let Some(init_stmt) = initializer {
            body = Stmt::Block(vec![init_stmt, body]);
        }

        Ok(body)
    }

    // ifStmt → "if" "(" expression ")" statement ( "else" statement )? ;
    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        // Consume 'if'
        self.advance();
        self.consume(TokenType::LEFT_PAREN, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN, "Expect ')' after if condition.")?;
        let then_branch = self.statement()?;
        let else_branch = if self.check(TokenType::ELSE) {
            self.advance();
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Stmt::If(Box::new(condition), Box::new(then_branch), else_branch.map(Box::new)))
    }

    // whileStmt → "while" "(" expression ")" statement ;
    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'while'
        self.consume(TokenType::LEFT_PAREN, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.")?;
        let body = self.statement()?;
        Ok(Stmt::While(Box::new(condition), Box::new(body)))
    }

    // returnStmt -> "return" expression? ";"
    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let keyword = self.advance().unwrap(); // consume 'return'
        let expr_opt = if !self.check(TokenType::SEMICOLON) {
            Some(Box::new(self.expression()?))
        } else { None };
        self.consume(TokenType::SEMICOLON, "Expect ';' after return value.")?;
        Ok(Stmt::Return(expr_opt))
    }

    // functionDecl -> "fun" IDENTIFIER "(" parameters? ")" block
    fn function_declaration(&mut self) -> Result<Stmt, ParseError> {
        // consume 'fun'
        self.advance();
        let name_token = self.consume(TokenType::IDENTIFIER, "Expect function name.")?;
        self.consume(TokenType::LEFT_PAREN, "Expect '(' after function name.")?;
        let mut params = Vec::new();
        if !self.check(TokenType::RIGHT_PAREN) {
            loop {
                if params.len() >= 255 {
                    return Err(ParseError::UnexpectedToken { expected: "<=255 params".into(), found: "more".into(), line: self.peek().unwrap().line });
                }
                params.push(self.consume(TokenType::IDENTIFIER, "Expect parameter name.")?);
                if !self.check(TokenType::COMMA) {
                    break;
                }
                self.advance(); // consume comma
            }
        }
        self.consume(TokenType::RIGHT_PAREN, "Expect ')' after parameters.")?;
        self.consume(TokenType::LEFT_BRACE, "Expect '{' before function body.")?;
        let body_stmts = self.block()?;
        Ok(Stmt::Function(name_token, params, body_stmts))
    }
}
