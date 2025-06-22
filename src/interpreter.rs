// use std::fmt::write; // unused
use crate::ast::{Expr, AstLiteralValue, Stmt, Program};
use crate::scanner::TokenType;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::callable::{LoxCallable, NativeFunction};
use std::rc::Rc;

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
    UndefinedVariable(String),
    IncorrectArgumentCount(usize, usize),
    Return(AstLiteralValue),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::OperandMustBeNumber => write!(f, "Operand must be a number."),
            RuntimeError::OperandsMustBeNumbers => write!(f, "Operands must be numbers."),
            RuntimeError::UnknownUnaryOperator => write!(f, "Unknown unary operator."),
            RuntimeError::UnknownBinaryOperator => write!(f, "Unknown binary operator."),
            RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable '{}' .", name),
            RuntimeError::IncorrectArgumentCount(expected, actual) => write!(f, "Expected {} arguments but got {}.", expected, actual),
            RuntimeError::Return(value) => write!(f, "Return statement with value: {:?}", value),
        }
    }
}

impl std::error::Error for RuntimeError {}

#[derive(Clone, Debug)]
pub struct Environment {
  values: RefCell<HashMap<String, AstLiteralValue>>,
  enclosing: Option<Box<Environment>>,
}

impl Environment {
  pub fn new() -> Self {
    Environment { values: RefCell::new(HashMap::new()),
       enclosing: None }
  }

  pub fn with_enclosing(enclosing: Environment) -> Self {
      Environment { values: RefCell::new(HashMap::new()), enclosing: Some(Box::new(enclosing)) }
  }

  pub fn define(&self, name: &str, value: AstLiteralValue) {
    self.values.borrow_mut().insert(name.to_string(), value);
  }

  pub fn get(&self, name: &str) -> Result<AstLiteralValue,RuntimeError> {
    if let Some(val) = self.values.borrow().get(name) {
      return Ok(val.clone());
    }
    if let Some(ref parent) = self.enclosing {
      return parent.get(name);
    }
    Err(RuntimeError::UndefinedVariable(name.to_string()))
  }
  pub fn assign(&self, name: &str, value: AstLiteralValue) -> Result<(), RuntimeError> {
    {
        let mut values = self.values.borrow_mut();
        if values.contains_key(name) {
            values.insert(name.to_string(), value.clone());
            return Ok(());
        }
    }
    if let Some(ref parent) = self.enclosing {
        return parent.assign(name, value);
    }
    Err(RuntimeError::UndefinedVariable(name.to_string()))
  }
}
pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Environment::new();
        let mut interpreter = Interpreter {
            environment: env,
        };
        interpreter.define_native_functions();
        interpreter
    }

    fn define_native_functions(&mut self) {
        let clock_fn = NativeFunction {
            name: "clock".to_string(),
            arity_val: 0,
            function: |_args| {
                use std::time::{SystemTime, UNIX_EPOCH};
                let since_epoch = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64();
                AstLiteralValue::Number(since_epoch)
            },
        };
        self.environment.define("clock", AstLiteralValue::Callable(Rc::new(clock_fn)));
    }

    pub fn execute_block_internal(&mut self, statements: &[Stmt], env: Environment) -> Result<(), RuntimeError> {
        let previous = self.environment.clone();
        self.environment = env;
        let mut result = Ok(());
        for stmt in statements {
            match self.execute(stmt) {
                Ok(()) => {},
                Err(e) => { result = Err(e); break; }
            }
        }
        self.environment = previous;
        result
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
            Stmt::VarDec(name_token, initializer) => {
                let value = if let Some(expr) = initializer {
                    self.evaluate(expr)?
                } else {
                    AstLiteralValue::Nil
                };
                self.environment.define(&name_token.lexeme, value);
                Ok(())
            }
            Stmt::Block(statements) => {
                let new_env = Environment {
                    values: RefCell::new(HashMap::new()),
                    enclosing: Some(Box::new(self.environment.clone())),
                };
                let previous = self.environment.clone();
                self.environment = new_env;
                for stmt in statements {
                    self.execute(stmt)?;
                }
                self.environment = previous;
                Ok(())
            }
            Stmt::If(condition, then_branch, else_branch) => {
                let cond_value = self.evaluate(condition)?;
                if is_truthy(&cond_value) {
                    self.execute(then_branch)?;
                } else if let Some(else_stmt) = else_branch {
                    self.execute(else_stmt)?;
                }
                Ok(())
            }
            Stmt::While(condition, body) => {
                while {
                    let cond_val = self.evaluate(condition)?;
                    is_truthy(&cond_val)
                } {
                    self.execute(body)?;
                }
                Ok(())
            }
            Stmt::Function(name_tok, params, body) => {
                let func = crate::callable::LoxFunction::new(
                    name_tok.lexeme.clone(),
                    params.clone(),
                    body.clone(),
                    self.environment.clone(),
                );
                self.environment.define(&name_tok.lexeme, AstLiteralValue::Callable(Rc::new(func)));
                Ok(())
            }
            Stmt::Return(expr_opt) => {
                let value = if let Some(expr) = expr_opt {
                    self.evaluate(expr)?
                } else {
                    AstLiteralValue::Nil
                };
                Err(RuntimeError::Return(value))
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
            Expr::Variable(name_token) => {
                self.environment.get(&name_token.lexeme)
            }
            Expr::Assign(name_token, value_expr) => {
                let value = self.evaluate(value_expr)?;
                self.environment.assign(&name_token.lexeme, value.clone())?;
                Ok(value)
            }
            Expr::Call(callee_expr, paren, arg_exprs) => {
                let callee_val = self.evaluate(callee_expr)?;
                let mut args = Vec::new();
                for arg_expr in arg_exprs {
                    args.push(self.evaluate(arg_expr)?);
                }
                match callee_val {
                    AstLiteralValue::Callable(func_rc) => {
                        let callable = func_rc;
                        if args.len() != callable.as_ref().arity() {
                            return Err(RuntimeError::IncorrectArgumentCount(callable.as_ref().arity(), args.len()));
                        }
                        callable.as_ref().call(self, args)
                    }
                    _ => {
                        println!("RuntimeError at line {}: Can only call functions.", paren.line);
                        Err(RuntimeError::UnknownUnaryOperator) // placeholder
                    }
                }
            }
        }
    }
}