use crate::ast::{Expr,AstLiteralValue};
use crate::scanner::{ TokenType};
// what i have to do in this file is to make a 
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
// our interpreter takes an 
pub fn evaluate(expr: &Expr) -> Result<AstLiteralValue, RuntimeError> {
    match expr {
        // 1) Literal: just clone and return it
        Expr::Literal(val) => Ok(val.clone()),

        // 2) Grouping: evaluate the inner expression
        Expr::Grouping(inner) => evaluate(inner),

        // 3) Unary: ! and - (negation)
        Expr::Unary(token, right_expr) => {
            // First, recursively evaluate the operand:
            let right_value = evaluate(right_expr)?;  
            match token.token_type {
                TokenType::BANG => {
                    // Logical NOT: wrap back into a Boolean literal
                    let result = !is_truthy(&right_value);
                    Ok(AstLiteralValue::Boolean(result))
                }
                TokenType::MINUS => {
                    // Numeric negation: check it really is a number
                    if let AstLiteralValue::Number(n) = right_value {
                        Ok(AstLiteralValue::Number(-n))
                    } else {
                        Err(RuntimeError::OperandMustBeNumber)
                    }
                }
                _ => Err(RuntimeError::UnknownUnaryOperator),
            }
        }

        // 4) Binary: +, -, *, /, comparisons, and/or, etc.
        Expr::Binary(left_expr, token, right_expr) => {
            // Evaluate both sides, propagating errors with `?`
            let left_value  = evaluate(left_expr)?;
            let right_value = evaluate(right_expr)?;
            match token.token_type {
                // Arithmetic
                TokenType::PLUS => {
                    match (&left_value, &right_value) {
                        (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) => {
                            Ok(AstLiteralValue::Number(*a + *b))
                        }
                        (AstLiteralValue::StringValue(s1), AstLiteralValue::StringValue(s2)) => {
                            Ok(AstLiteralValue::StringValue(s1.clone() + s2.as_str()))
                        }
                        _ => {
                            Err(RuntimeError::OperandsMustBeNumbers)
                        }
                    }
                }
                TokenType::MINUS => {
                    if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (left_value, right_value) {
                        Ok(AstLiteralValue::Number(a - b))
                    } else {
                        Err(RuntimeError::OperandsMustBeNumbers)
                    }
                }
                TokenType::STAR => {
                    if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (left_value, right_value) {
                        Ok(AstLiteralValue::Number(a * b))
                    } else {
                        Err(RuntimeError::OperandsMustBeNumbers)
                    }
                }
                TokenType::SLASH => {
                    if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (left_value, right_value) {
                        Ok(AstLiteralValue::Number(a / b))
                    } else {
                        Err(RuntimeError::OperandsMustBeNumbers)
                    }
                }
                // Comparison
                TokenType::GREATER => {
                    if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (left_value, right_value) {
                        Ok(AstLiteralValue::Boolean(a > b))
                    } else {
                        Err(RuntimeError::OperandsMustBeNumbers)
                    }
                }
                TokenType::GREATER_EQUAL => {
                    if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (left_value, right_value) {
                        Ok(AstLiteralValue::Boolean(a >= b))
                    } else {
                        Err(RuntimeError::OperandsMustBeNumbers)
                    }
                }
                TokenType::LESS => {
                    if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (left_value, right_value) {
                        Ok(AstLiteralValue::Boolean(a < b))
                    } else {
                        Err(RuntimeError::OperandsMustBeNumbers)
                    }
                }
                TokenType::LESS_EQUAL => {
                    if let (AstLiteralValue::Number(a), AstLiteralValue::Number(b)) = (left_value, right_value) {
                        Ok(AstLiteralValue::Boolean(a <= b))
                    } else {
                        Err(RuntimeError::OperandsMustBeNumbers)
                    }
                }
                // Equality
                TokenType::EQUAL_EQUAL => Ok(AstLiteralValue::Boolean(left_value == right_value)),
                TokenType::BANG_EQUAL  => Ok(AstLiteralValue::Boolean(left_value != right_value)),
                // Logical
                TokenType::OR  => Ok(AstLiteralValue::Boolean(is_truthy(&left_value) || is_truthy(&right_value))),
                TokenType::AND => Ok(AstLiteralValue::Boolean(is_truthy(&left_value) && is_truthy(&right_value))),
                _ => Err(RuntimeError::UnknownBinaryOperator),
            }
        }
    }
}