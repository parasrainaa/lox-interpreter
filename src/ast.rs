use crate::scanner::Token; 
use std::fmt;

#[derive(Debug, Clone, PartialEq,)]
pub enum AstLiteralValue {
    Number(f64),
    StringValue(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(AstLiteralValue),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>), 
    Variable(Token),
    Assign(Token, Box<Expr>)
}
#[derive(Debug,Clone)]
pub enum Stmt {
  ExprStmt(Box<Expr>),
  PrintStmt(Box<Expr>),
  VarDec(Token,Box<Expr>),
  Block(Vec<Stmt>),
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
      Stmt::VarDec(name, expr) => write!(f, "var {} = {}", name.lexeme, expr),
      Stmt::Block(statements) => {
        write!(f, "(block")?;
        for stmt in statements {
          write!(f, " {}", stmt)?;
        }
        write!(f, ")")
      }
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
            Expr::Variable(name) => {
                write!(f, "{}", name.lexeme)
            }
            Expr::Assign(name, value) => {
                write!(f, "{} = {}", name.lexeme, value)
            }
        }
    }
}