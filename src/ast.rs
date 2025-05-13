use crate::scanner::Token; // For operator in Binary/Unary expressions
use std::fmt;

// Represents the different kinds of literal values in the AST.
#[derive(Debug, Clone)]
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

impl fmt::Display for AstLiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstLiteralValue::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{:.1}", n)
                } else {
                    write!(f, "{}", n)
                }
            }
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
            Expr::Literal(value) => write!(f, "{}", value),
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