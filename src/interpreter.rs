use crate::ast::{Expr, AstLiteralValue, Stmt, Program};
use crate::scanner::TokenType;

fn is_truthy(value: &AstLiteralValue) -> bool {
    match value {
        AstLiteralValue::Nil => false,
        AstLiteralValue::Boolean(b) => *b,
        _ => true,
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    OperandMustBeNumber,
    OperandsMustBeNumbers,
    UnknownUnaryOperator,
    UnknownBinaryOperator,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::OperandMustBeNumber => write!(f, "Operand must be a number."),
            RuntimeError::OperandsMustBeNumbers => write!(f, "Operands must be numbers."),
            RuntimeError::UnknownUnaryOperator => write!(f, "Unknown unary operator."),
            RuntimeError::UnknownBinaryOperator => write!(f, "Unknown binary operator."),
        }
    }
}

impl std::error::Error for RuntimeError {}

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn interpret(&mut self, program: &Program) -> Result<(), RuntimeError> {
        for statement in &program.statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::ExprStmt(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::PrintStmt(expr) => {
                let value = self.evaluate(expr)?;
                println!("{}", value);
                Ok(())
            }
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<AstLiteralValue, RuntimeError> {
        match expr {
            Expr::Literal(val) => Ok(val.clone()),
            Expr::Grouping(inner) => self.evaluate(inner),
            Expr::Unary(token, right_expr) => {
                let right_value = self.evaluate(right_expr)?;
                match token.token_type {
                    TokenType::BANG => {
                        let result = !is_truthy(&right_value);
                        Ok(AstLiteralValue::Boolean(result))
                    }
                    TokenType::MINUS => {
                        if let AstLiteralValue::Number(n) = right_value {
                            Ok(AstLiteralValue::Number(-n))
                        } else {
                            Err(RuntimeError::OperandMustBeNumber)
                        }
                    }
                    _ => Err(RuntimeError::UnknownUnaryOperator),
                }
            }
            Expr::Binary(left_expr, token, right_expr) => {
                let left_value = self.evaluate(left_expr)?;
                let right_value = self.evaluate(right_expr)?;
                match token.token_type {
                    TokenType::PLUS => {
                        match (&left_value, &right_value) {
                            (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) => {
                                Ok(AstLiteralValue::Number(*a + *b))
                            }
                            (AstLiteralValue::StringValue(s1), AstLiteralValue::StringValue(s2)) => {
                                Ok(AstLiteralValue::StringValue(s1.clone() + s2.as_str()))
                            }
                            _ => Err(RuntimeError::OperandsMustBeNumbers),
                        }
                    }
                    TokenType::MINUS => {
                        if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (&left_value, &right_value) {
                            Ok(AstLiteralValue::Number(a - b))
                        } else {
                            Err(RuntimeError::OperandsMustBeNumbers)
                        }
                    }
                    TokenType::STAR => {
                        if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (&left_value, &right_value) {
                            Ok(AstLiteralValue::Number(a * b))
                        } else {
                            Err(RuntimeError::OperandsMustBeNumbers)
                        }
                    }
                    TokenType::SLASH => {
                        if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (&left_value, &right_value) {
                            if *b == 0.0 {
                                println!("RuntimeError: Division by zero.");
                                Err(RuntimeError::OperandsMustBeNumbers)
                            } else {
                                Ok(AstLiteralValue::Number(a / b))
                            }
                        } else {
                            Err(RuntimeError::OperandsMustBeNumbers)
                        }
                    }
                    TokenType::GREATER => {
                        if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (&left_value, &right_value) {
                            Ok(AstLiteralValue::Boolean(a > b))
                        } else {
                            Err(RuntimeError::OperandsMustBeNumbers)
                        }
                    }
                    TokenType::GREATER_EQUAL => {
                        if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (&left_value, &right_value) {
                            Ok(AstLiteralValue::Boolean(a >= b))
                        } else {
                            Err(RuntimeError::OperandsMustBeNumbers)
                        }
                    }
                    TokenType::LESS => {
                        if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (&left_value, &right_value) {
                            Ok(AstLiteralValue::Boolean(a < b))
                        } else {
                            Err(RuntimeError::OperandsMustBeNumbers)
                        }
                    }
                    TokenType::LESS_EQUAL => {
                        if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (&left_value, &right_value) {
                            Ok(AstLiteralValue::Boolean(a <= b))
                        } else {
                            Err(RuntimeError::OperandsMustBeNumbers)
                        }
                    }
                    TokenType::EQUAL_EQUAL => Ok(AstLiteralValue::Boolean(left_value == right_value)),
                    TokenType::BANG_EQUAL => Ok(AstLiteralValue::Boolean(left_value != right_value)),
                    TokenType::OR => Ok(AstLiteralValue::Boolean(is_truthy(&left_value) || is_truthy(&right_value))),
                    TokenType::AND => Ok(AstLiteralValue::Boolean(is_truthy(&left_value) && is_truthy(&right_value))),
                    _ => Err(RuntimeError::UnknownBinaryOperator),
                }
            }
        }
    }
}