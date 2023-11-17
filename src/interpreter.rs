use crate::environment::Environment;
use crate::stmt::Stmt;

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, stmt: Vec<Stmt>) -> Result<(), std::io::Error> {
        for stmt in stmt {
            match stmt {
                Stmt::Expression { expression } => {
                    expression.evaluate(&mut self.environment)?;
                }
                Stmt::Print { expression } => {
                    let value = expression.evaluate(&mut self.environment)?;
                    println!("{}", value);
                }
                Stmt::Var { name, initializer } => {
                    let value = initializer.evaluate(&mut self.environment)?;
                    self.environment.define(name.lexeme, value.clone());
                }
            };
        }
        Ok(())
    }
}
