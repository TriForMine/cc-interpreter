use crate::environment::Environment;
use crate::expr::{Expr, LiteralValue};
use crate::expr::LiteralValue::Callable;
use crate::stmt::Stmt;
use std::cell::RefCell;
use std::rc::Rc;
use crate::scanner::Token;
use anyhow::Result;

pub struct Interpreter {
    pub(crate) specials: Rc<RefCell<Environment>>,
    pub(crate) environment: Rc<RefCell<Environment>>,
}

fn clock_impl(_args: &Vec<LiteralValue>) -> LiteralValue {
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
            specials: Rc::new(RefCell::new(Environment::new())),
            //environment: Rc::new(RefCell::new(Environment::new())),
            environment: Rc::new(RefCell::new(globals)),
        }
    }

    fn for_closure(parent: Rc<RefCell<Environment>>) -> Self {
        Interpreter {
            specials: Rc::new(RefCell::new(Environment::new())),
            environment: Rc::from(RefCell::new(Environment::new_with_enclosing(parent))),
        }
    }

    pub fn for_anonymous(parent: Rc<RefCell<Environment>>) -> Self {
        Interpreter {
            specials: Rc::new(RefCell::new(Environment::new())),
            environment: Rc::new(RefCell::new(Environment::new_with_enclosing(parent))),
        }
    }

    pub fn interpret(&mut self, stmt: Vec<&Stmt>) -> Result<()> {
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
                Stmt::If {
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
                Stmt::While { condition, body } => {
                    let mut flag = condition.evaluate(self.environment.clone())?;

                    while flag.is_truthy() {
                        self.interpret(vec![body])?;
                        flag = condition.evaluate(self.environment.clone())?;
                    }
                }
                Stmt::Function { name, params, body } => {
                    let arity = params.len();

                    let params: Vec<Token> = params.iter().map(|x| (*x).clone()).collect();
                    let body: Vec<Box<Stmt>> = body.iter().map(|x| (*x).clone()).collect();

                    let parent_env = self.environment.clone();

                    let fun_impl = move |args: &Vec<LiteralValue>| {
                        let mut closure_interpreter = Interpreter::for_closure(parent_env.clone());

                        for (i, arg) in args.iter().enumerate() {
                            closure_interpreter
                                .environment
                                .borrow_mut()
                                .define(params[i].lexeme.clone(), (*arg).clone());
                        }

                        for i in 0..body.len() {
                            closure_interpreter
                                .interpret(vec![&body[i].as_ref()])
                                .expect("Error in function body");

                            if let Some(return_value) = closure_interpreter.specials.borrow().get("return") {
                                return return_value.clone();
                            }
                        }

                        LiteralValue::Nil
                    };

                    let function = Callable {
                        name: name.lexeme.clone(),
                        arity,
                        fun: Rc::new(fun_impl),
                    };

                    self.environment.borrow_mut().define(name.lexeme.clone(), function)
                }
                Stmt::Return { keyword: _, value } => {
                    let value = value
                        .as_ref()
                        .map(|x| x.evaluate(self.environment.clone()))
                        .unwrap_or(Ok(LiteralValue::Nil))?;

                    self.specials.borrow_mut().define_top_level("return".to_string(), value);
                }
            };
        };

        Ok(())
    }

    pub fn resolve(&mut self, expr: &Expr, steps: usize) -> Result<()> {
        todo!()
    }
}
