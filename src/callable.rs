pub trait LoxCallable: std::fmt::Debug {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut crate::interpreter::Interpreter, arguments: Vec<crate::ast::AstLiteralValue>) -> Result<crate::ast::AstLiteralValue, crate::interpreter::RuntimeError>;
    fn to_string(&self) -> String;
}

use crate::scanner::Token;
use crate::ast::{Stmt, AstLiteralValue};
use crate::interpreter::{Interpreter, RuntimeError};

#[derive(Debug, Clone)]
pub struct LoxFunction {
    declaration: Stmt,           // Must be Stmt::Function variant
    name: String,
    params: Vec<Token>,
    body: Vec<Stmt>,
    closure: crate::interpreter::Environment,
}

impl LoxFunction {
    pub fn new(name: String, params: Vec<Token>, body: Vec<Stmt>, closure: crate::interpreter::Environment) -> Self {
        LoxFunction { declaration: Stmt::Block(body.clone()), name, params, body, closure }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<AstLiteralValue>) -> Result<AstLiteralValue, RuntimeError> {
        // Create new environment with closure as parent
        let new_env = crate::interpreter::Environment::with_enclosing(self.closure.clone());

        for (i, param) in self.params.iter().enumerate() {
            new_env.define(&param.lexeme, arguments[i].clone());
        }

        // Execute body in new environment
        match interpreter.execute_block_internal(&self.body, new_env) {
            Ok(()) => Ok(AstLiteralValue::Nil),
            Err(RuntimeError::Return(value)) => Ok(value),
            Err(e) => Err(e),
        }
    }

    fn to_string(&self) -> String {
        format!("<fn {}>", self.name)
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity_val: usize,
    pub function: fn(Vec<AstLiteralValue>) -> AstLiteralValue,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn {}>", self.name)
    }
}

impl LoxCallable for NativeFunction {
    fn arity(&self) -> usize { self.arity_val }

    fn call(&self, _interpreter: &mut Interpreter, arguments: Vec<AstLiteralValue>) -> Result<AstLiteralValue, RuntimeError> {
        if arguments.len() != self.arity_val {
            return Err(RuntimeError::IncorrectArgumentCount(self.arity_val, arguments.len()));
        }
        Ok((self.function)(arguments))
    }

    fn to_string(&self) -> String {
        format!("<native fn {}>", self.name)
    }
} 