use crate::environment::Environment;
use crate::expr::LiteralValue;
use crate::expr::LiteralValue::Callable;
use crate::stmt::Stmt;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter {
    //globals: Environment,
    environment: Rc<RefCell<Environment>>,
}

fn clock_impl(_environement: Rc<RefCell<Environment>>, _args: &Vec<LiteralValue>) -> LiteralValue {
    LiteralValue::Float(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Environment::new();

        globals.define(
            "clock".to_string(),
            Callable {
                name: "clock".to_string(),
                arity: 0,
                fun: Rc::new(clock_impl),
            },
        );

        Interpreter {
            //globals,
            //environment: Rc::new(RefCell::new(Environment::new())),
            environment: Rc::new(RefCell::new(globals)),
        }
    }

    fn for_closure(parent_env: Rc<RefCell<Environment>>) -> Self {
        Interpreter {
            environment: parent_env,
        }
    }

    pub fn interpret(&mut self, stmt: Vec<&Stmt>) -> Result<(), std::io::Error> {
        for stmt in stmt {
            match stmt {
                Stmt::Expression { expression } => {
                    expression.evaluate(self.environment.clone())?;
                }
                Stmt::Print { expression } => {
                    let value = expression.evaluate(self.environment.clone())?;
                    println!("{}", value.to_string());
                }
                Stmt::Var { name, initializer } => {
                    let value = initializer.evaluate(self.environment.clone())?;
                    self.environment
                        .borrow_mut()
                        .define(name.lexeme.clone(), value);
                }
                Stmt::Block { statements } => {
                    let new_env = Environment::new_with_enclosing(self.environment.clone());

                    let old_env = self.environment.clone();
                    self.environment = Rc::new(RefCell::new(new_env));

                    let result = self.interpret(statements.iter().map(|x| x.as_ref()).collect());

                    self.environment = old_env;

                    result?;
                }
                Stmt::IfStmt {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let condition = condition.evaluate(self.environment.clone())?;

                    if condition.is_truthy() {
                        self.interpret(vec![then_branch])?;
                    } else if let Some(else_branch) = else_branch {
                        self.interpret(vec![else_branch])?;
                    }
                }
                Stmt::WhileStmt { condition, body } => {
                    let mut flag = condition.evaluate(self.environment.clone())?;

                    while flag.is_truthy() {
                        self.interpret(vec![body])?;
                        flag = condition.evaluate(self.environment.clone())?;
                    }
                }
                Stmt::Function { name, params, body } => {
                    let fun_impl = |parent_env, args: &Vec<LiteralValue>| {
                        let mut closure_interpreter = Interpreter::for_closure(parent_env);

                        for (param, arg) in params.iter().zip(args.iter()) {
                            closure_interpreter
                                .environment
                                .borrow_mut()
                                .define(param.lexeme.clone(), arg.clone());
                        }

                        for i in 0..body.len() - 1 {
                            closure_interpreter
                                .interpret(vec![&body[i].as_ref()])
                                .unwrap();
                        }

                        let value;
                        match body.last() {
                            Some(box Stmt::Expression { expression }) => {
                                value = expression
                                    .evaluate(closure_interpreter.environment)
                                    .unwrap();
                            }
                            _ => {
                                value = LiteralValue::Nil;
                            }
                        }

                        value
                    };

                    let function = Callable {
                        name: name.lexeme.clone(),
                        arity: params.len(),
                        fun: Rc::new(fun_impl),
                    };

                    if !self.environment.borrow_mut().assign(&name.lexeme, function) {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            format!("Could not define function {}", name.lexeme),
                        ));
                    }
                }
            };
        }
        Ok(())
    }
}
