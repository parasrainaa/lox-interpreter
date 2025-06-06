use crate::scanner::Token; // For operator in Binary/Unary expressions
use std::fmt;

// Represents the different kinds of literal values in the AST.
#[derive(Debug, Clone, PartialEq,)]
pub enum AstLiteralValue {
    Number(f64),
    StringValue(String),
    Boolean(bool),
    Nil,
}

// Represents expressions in the language.
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(AstLiteralValue),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>), 
}
#[derive(Debug,Clone)]
pub enum Stmt {
  ExprStmt(Box<Expr>),
  PrintStmt(Box<Expr>)
}
#[derive(Debug,Clone)]
pub struct Program {
  pub statements : Vec<Stmt>,
}
impl Program {
  pub fn new(statements: Vec<Stmt>) -> Self {
    Program{statements}
  }
}
impl fmt::Display for Stmt {
  fn fmt(&self,f:&mut fmt::Formatter<'_>) -> fmt::Result {
    match self{
      Stmt::ExprStmt(expr) => write!(f, "expr-stmt {}", expr),
      Stmt::PrintStmt(expr) => write!(f, "print-stmt {}",expr),
    }
  }
}
impl fmt::Display for Program {
  fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result{
    for stmt in &self.statements{
      writeln!(f,"{}",stmt)?;
    } 
    Ok(())
  }
}
impl fmt::Display for AstLiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstLiteralValue::Number(n) => write!(f, "{}", n),
            AstLiteralValue::StringValue(s) => write!(f, "{}", s), 
            AstLiteralValue::Boolean(b) => write!(f, "{}", b),
            AstLiteralValue::Nil => write!(f, "nil"),
        }
    }
}

// Helper function to parenthesize expressions for display, common in Lox interpreters.
// For example, a binary + expression with 1 and 2 would be (+ 1.0 2.0)
fn parenthesize_operator(f: &mut fmt::Formatter<'_>, operator_lexeme: &str, exprs: &[&Box<Expr>]) -> fmt::Result {
    write!(f, "({}", operator_lexeme)?;
    for expr in exprs {
        write!(f, " {}", expr)?;
    }
    write!(f, ")")
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(value) => {
                match value {
                    AstLiteralValue::Number(n) if n.fract() == 0.0 => write!(f, "{:.1}", n),
                    AstLiteralValue::Number(n) => write!(f, "{}", n),
                    other => write!(f, "{}", other),
                }
            }
            Expr::Grouping(expression) => {
                write!(f, "(group {})", expression)
            }
            Expr::Unary(operator_token, right_expr) => {
                parenthesize_operator(f, &operator_token.lexeme, &[right_expr])
            }
            Expr::Binary(left_expr, operator_token, right_expr) => {
                parenthesize_operator(f, &operator_token.lexeme, &[left_expr, right_expr])
            }
        }
    }
}