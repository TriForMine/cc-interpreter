use crate::environment::Environment;
use crate::expr::{CallableImpl, FunctionImpl, LiteralValue, NativeFunctionImpl};
use crate::scanner::Token;
use crate::stmt::Stmt;
use anyhow::{bail, Result};
use std::collections::HashMap;
use std::process::Command;
use std::rc::Rc;

pub struct Interpreter {
    pub specials: HashMap<String, LiteralValue>,
    pub environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            specials: HashMap::new(),
            environment: Environment::new(),
        }
    }

    pub fn resolve(&mut self, locals: HashMap<usize, usize>) {
        self.environment.resolve(locals);
    }

    pub fn with_environment(environment: Environment) -> Self {
        Interpreter {
            specials: HashMap::new(),
            environment,
        }
    }

    #[allow(dead_code)]
    pub fn for_anonymous(parent: Environment) -> Self {
        Interpreter {
            specials: HashMap::new(),
            environment: parent.enclose(),
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
                    println!("{}", value);
                }
                Stmt::Var { name, initializer } => {
                    let value = initializer.evaluate(self.environment.clone())?;

                    self.environment.define(name.lexeme.clone(), value);
                }
                Stmt::Block { statements } => {
                    let new_env = self.environment.enclose();

                    let old_env = self.environment.clone();
                    self.environment = new_env;

                    let result = self.interpret(statements.iter().map(|x| x.as_ref()).collect());

                    self.environment = old_env;

                    result?;
                }
                Stmt::Class {
                    name,
                    methods,
                    superclass,
                    attributes,
                } => {
                    let mut methods_map: HashMap<String, FunctionImpl> = HashMap::new();
                    let mut attributes_map: HashMap<String, LiteralValue> = HashMap::new();

                    let superclass_value = if let Some(superclass) = superclass {
                        let superclass = superclass.evaluate(self.environment.clone())?;

                        if let LiteralValue::Class { methods: _, .. } = superclass {
                            Some(Box::new(superclass))
                        } else {
                            bail!(
                                "Superclass must be a class, not a {}",
                                superclass.type_name()
                            )
                        }
                    } else {
                        None
                    };

                    self.environment
                        .define(name.lexeme.clone(), LiteralValue::Nil);

                    self.environment = self.environment.enclose();
                    if let Some(superclass) = superclass_value.clone() {
                        self.environment.define("super".to_string(), *superclass);
                    }

                    for method in methods {
                        if let Stmt::Function { name, .. } = method.as_ref() {
                            let function = self.make_function(method.as_ref());
                            methods_map.insert(name.lexeme.clone(), function);
                        } else {
                            bail!("Error in class definition")
                        }
                    }

                    for attribute in attributes {
                        if let Stmt::Var { name, initializer } = attribute {
                            let value = initializer.evaluate(self.environment.clone())?;
                            attributes_map.insert(name.lexeme.clone(), value);
                        } else {
                            bail!("Error in class definition")
                        }
                    }

                    let class = LiteralValue::Class {
                        name: name.lexeme.clone(),
                        methods: methods_map,
                        superclass: superclass_value,
                        attributes: attributes_map,
                    };

                    if !self.environment.assign_global(&name.lexeme, class) {
                        bail!("Error in class definition")
                    }

                    self.environment = *self.environment.enclosing.clone().unwrap();
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
                Stmt::Function { name, .. } => {
                    let callable = self.make_function(stmt);
                    let fun = LiteralValue::Callable(CallableImpl::Function(callable));
                    self.environment.define(name.lexeme.clone(), fun);
                }
                Stmt::Return { keyword: _, value } => {
                    let value = value
                        .as_ref()
                        .map(|x| x.evaluate(self.environment.clone()))
                        .transpose()?;

                    self.specials
                        .insert("return".to_string(), value.unwrap_or(LiteralValue::Nil));
                }
                Stmt::CmdFunction { name, cmd } => {
                    let cmd = cmd.clone();

                    let local_fn = move |_args: &Vec<LiteralValue>| {
                        let mut cmd = cmd.clone();
                        let mut args = _args.clone();
                        let mut args_str = String::new();

                        for arg in args.iter() {
                            args_str.push_str(&arg.to_string());
                            args_str.push(' ');
                        }

                        cmd.push_str(&args_str);

                        let output = Command::new("sh")
                            .arg("-c")
                            .arg(cmd)
                            .output()
                            .expect("failed to execute process");

                        let output = String::from_utf8(output.stdout).unwrap();
                        let output = output.trim().to_string();

                        LiteralValue::String(output)
                    };

                    let fun =
                        LiteralValue::Callable(CallableImpl::NativeFunction(NativeFunctionImpl {
                            name: name.lexeme.clone(),
                            arity: 0,
                            fun: Rc::new(local_fn),
                        }));
                    self.environment.define(name.lexeme.clone(), fun);
                }
            };
        }

        Ok(())
    }

    fn make_function(&self, fn_stmt: &Stmt) -> FunctionImpl {
        match fn_stmt {
            Stmt::Function { name, params, body } => {
                let arity = params.len();
                let params: Vec<Token> = params.iter().map(|x| (*x).clone()).collect();
                let body: Vec<Box<Stmt>> = body.iter().map(|x| (*x).clone()).collect();
                let parent_env = self.environment.clone();

                let callable_impl = FunctionImpl {
                    name: name.lexeme.clone(),
                    arity,
                    params,
                    body,
                    parent_env,
                };

                callable_impl
            }
            _ => panic!("Error in function definition"),
        }
    }
}
