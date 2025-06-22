use crate::scanner::Token; 
use std::fmt;
use std::rc::Rc;
use crate::callable::LoxCallable;

#[derive(Debug, Clone)]
pub enum AstLiteralValue {
    Number(f64),
    StringValue(String),
    Boolean(bool),
    Nil,
    Callable(Rc<dyn LoxCallable>),
}

impl PartialEq for AstLiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) => a == b,
            (AstLiteralValue::StringValue(a), AstLiteralValue::StringValue(b)) => a == b,
            (AstLiteralValue::Boolean(a), AstLiteralValue::Boolean(b)) => a == b,
            (AstLiteralValue::Nil, AstLiteralValue::Nil) => true,
            (AstLiteralValue::Callable(a), AstLiteralValue::Callable(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(AstLiteralValue),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>), 
    Binary(Box<Expr>, Token, Box<Expr>), 
    Variable(Token),
    Assign(Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
}
#[derive(Debug,Clone)]
pub enum Stmt {
  ExprStmt(Box<Expr>),
  PrintStmt(Box<Expr>),
  VarDec(Token,Option<Box<Expr>>),
  Block(Vec<Stmt>),
  If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
  While(Box<Expr>, Box<Stmt>),
  Function(Token, Vec<Token>, Vec<Stmt>),
  Return(Option<Box<Expr>>),
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
      Stmt::VarDec(name, initializer) => {
        if let Some(expr) = initializer {
          write!(f, "var {} = {}", name.lexeme, expr)
        } else {
          write!(f, "var {}", name.lexeme)
        }
      },
      Stmt::Block(statements) => {
        write!(f, "(block")?;
        for stmt in statements {
          write!(f, " {}", stmt)?;
        }
        write!(f, ")")
      },
      Stmt::If(condition, then_branch, else_branch) => {
        if let Some(else_stmt) = else_branch {
          write!(f, "(if {} then {} else {})", condition, then_branch, else_stmt)
        } else {
          write!(f, "(if {} then {})", condition, then_branch)
        }
      },
      Stmt::While(condition, body) => {
        write!(f, "(while {} {})", condition, body)
      },
      Stmt::Function(name, _params, _body) => {
        write!(f, "<fn {}>", name.lexeme)
      },
      Stmt::Return(expr_opt) => {
        if let Some(expr) = expr_opt {
          write!(f, "return {}", expr)
        } else {
          write!(f, "return")
        }
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
            AstLiteralValue::Callable(c) => write!(f, "{}", c.as_ref().to_string()),
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
            Expr::Call(callee, _paren, arguments) => {
                write!(f, "(call {}", callee)?;
                for arg in arguments {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}