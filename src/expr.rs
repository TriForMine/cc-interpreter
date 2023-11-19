use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::scanner::{Token, TokenType};
use crate::stmt::Stmt;
use anyhow::{bail, Result};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone, Hash)]
pub enum CallableImpl {
    Function(FunctionImpl),
    NativeFunction(NativeFunctionImpl),
}

#[derive(Clone, Hash)]
pub struct FunctionImpl {
    pub name: String,
    pub arity: usize,
    pub parent_env: Environment,
    pub params: Vec<Token>,
    pub body: Vec<Box<Stmt>>,
}

#[derive(Clone)]
pub struct NativeFunctionImpl {
    pub name: String,
    pub arity: usize,
    pub fun: Rc<dyn Fn(&Vec<LiteralValue>) -> LiteralValue>,
}

impl Hash for NativeFunctionImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        "native".hash(state);
        self.name.hash(state);
    }
}

#[derive(Clone)]
pub enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
    Callable(CallableImpl),
    Class {
        name: String,
        methods: HashMap<String, FunctionImpl>,
        superclass: Option<Box<LiteralValue>>,
        attributes: HashMap<String, LiteralValue>,
    },
    Instance {
        class: Box<LiteralValue>,
        fields: Rc<RefCell<HashMap<String, LiteralValue>>>,
    },
}

impl Eq for LiteralValue {}

impl Hash for LiteralValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LiteralValue::Integer(i) => {
                "int".hash(state);
                i.hash(state);
            }
            LiteralValue::Float(f) => {
                "float".hash(state);
                f.to_bits().hash(state);
            }
            LiteralValue::String(s) => {
                "string".hash(state);
                s.hash(state);
            }
            LiteralValue::Boolean(b) => {
                "bool".hash(state);
                b.hash(state);
            }
            LiteralValue::Nil => {
                "nil".hash(state);
            }
            LiteralValue::Callable(CallableImpl::Function(FunctionImpl { name, .. })) => {
                "function".hash(state);
                name.hash(state);
            }
            LiteralValue::Callable(CallableImpl::NativeFunction(NativeFunctionImpl {
                name,
                ..
            })) => {
                "native".hash(state);
                name.hash(state);
            }
            LiteralValue::Class {
                name,
                methods,
                superclass,
                attributes,
            } => {
                "class".hash(state);
                name.hash(state);

                for (name, method) in methods {
                    name.hash(state);
                    method.hash(state);
                }

                if let Some(superclass) = superclass {
                    superclass.hash(state);
                }

                for (name, value) in attributes {
                    name.hash(state);
                    value.hash(state);
                }
            }
            LiteralValue::Instance {
                class: class_name, ..
            } => {
                "instance".hash(state);
                class_name.hash(state);
            }
        }
    }
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralValue::Integer(l), LiteralValue::Integer(r)) => l == r,
            (LiteralValue::Float(l), LiteralValue::Float(r)) => l == r,
            (LiteralValue::String(l), LiteralValue::String(r)) => l == r,
            (LiteralValue::Boolean(l), LiteralValue::Boolean(r)) => l == r,
            (LiteralValue::Nil, LiteralValue::Nil) => true,
            (LiteralValue::Callable { .. }, LiteralValue::Callable { .. }) => {
                panic!("Cannot compare functions")
            }
            _ => false,
        }
    }
}

impl PartialOrd for LiteralValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (LiteralValue::Integer(l), LiteralValue::Integer(r)) => l.partial_cmp(r),
            (LiteralValue::Float(l), LiteralValue::Float(r)) => l.partial_cmp(r),
            (LiteralValue::String(l), LiteralValue::String(r)) => l.partial_cmp(r),
            (LiteralValue::Boolean(l), LiteralValue::Boolean(r)) => l.partial_cmp(r),
            (LiteralValue::Nil, LiteralValue::Nil) => Some(std::cmp::Ordering::Equal),
            (LiteralValue::Callable { .. }, LiteralValue::Callable { .. }) => {
                panic!("Cannot compare functions")
            }
            _ => None,
        }
    }
}

impl LiteralValue {
    pub fn to_string(&self) -> String {
        match self {
            LiteralValue::Integer(i) => format!("{}", i),
            LiteralValue::Float(f) => format!("{}", f),
            LiteralValue::String(s) => format!("\"{}\"", s),
            LiteralValue::Boolean(b) => format!("{}", b),
            LiteralValue::Nil => format!("nil"),
            LiteralValue::Callable(CallableImpl::Function(FunctionImpl { name, .. })) => {
                format!("<fn {}>", name)
            }
            LiteralValue::Callable(CallableImpl::NativeFunction(NativeFunctionImpl {
                name,
                ..
            })) => {
                format!("<native fn {}>", name)
            }
            LiteralValue::Class { name, .. } => format!("<class {}>", name),
            LiteralValue::Instance { class, fields: _ } => {
                if let LiteralValue::Class { name, .. } = &**class {
                    format!("<instance of {}>", name)
                } else {
                    panic!("Expected a class")
                }
            }
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            LiteralValue::Integer(_) => "int".to_string(),
            LiteralValue::Float(_) => "float".to_string(),
            LiteralValue::String(_) => "string".to_string(),
            LiteralValue::Boolean(_) => "bool".to_string(),
            LiteralValue::Nil => "nil".to_string(),
            LiteralValue::Callable(CallableImpl::Function(FunctionImpl { name, .. })) => {
                format!("function {}", name)
            }
            LiteralValue::Callable(CallableImpl::NativeFunction(NativeFunctionImpl {
                name,
                ..
            })) => {
                format!("native {}", name)
            }
            LiteralValue::Class { name, .. } => format!("class {}", name),
            LiteralValue::Instance { class, fields: _ } => {
                if let LiteralValue::Class { name, .. } = &**class {
                    format!("instance of {}", name)
                } else {
                    panic!("Expected a class")
                }
            }
        }
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            LiteralValue::Integer(l) => *l == 0,
            LiteralValue::Float(l) => *l == 0.0,
            LiteralValue::String(l) => l.is_empty(),
            LiteralValue::Boolean(l) => !*l,
            LiteralValue::Nil => true,
            LiteralValue::Callable { .. } => {
                panic!("Cannot check if a function is falsey")
            }
            LiteralValue::Class { .. } => {
                panic!("Cannot check if a class is falsey")
            }
            LiteralValue::Instance { .. } => {
                panic!("Cannot check if an instance is falsey")
            }
        }
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsey()
    }
}

impl Debug for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expr {
    AnonFunction {
        id: usize,
        arguments: Vec<Token>,
        paren: Token,
        body: Vec<Box<Stmt>>,
    },

    Assign {
        id: usize,
        name: Token,
        value: Box<Expr>,
    },

    Binary {
        id: usize,
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },

    Call {
        id: usize,
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },

    Get {
        id: usize,
        object: Box<Expr>,
        name: Token,
    },

    Grouping {
        id: usize,
        expression: Box<Expr>,
    },

    Literal {
        id: usize,
        value: LiteralValue,
    },

    Logical {
        id: usize,
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },

    Set {
        id: usize,
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },

    Super {
        id: usize,
        keyword: Token,
        method: Token,
    },

    This {
        id: usize,
        keyword: Token,
    },

    TypeOf {
        id: usize,
        expression: Box<Expr>,
    },

    Unary {
        id: usize,
        operator: Token,
        right: Box<Expr>,
    },

    Variable {
        id: usize,
        name: Token,
    },
}

impl Expr {
    pub fn get_id(&self) -> usize {
        match self {
            Expr::AnonFunction { id, .. } => *id,
            Expr::Assign { id, .. } => *id,
            Expr::Binary { id, .. } => *id,
            Expr::Call { id, .. } => *id,
            Expr::Grouping { id, .. } => *id,
            Expr::Literal { id, .. } => *id,
            Expr::Logical { id, .. } => *id,
            Expr::Unary { id, .. } => *id,
            Expr::Variable { id, .. } => *id,
            Expr::Get { id, .. } => *id,
            Expr::Set { id, .. } => *id,
            Expr::This { id, .. } => *id,
            Expr::Super { id, .. } => *id,
            Expr::TypeOf { id, .. } => *id,
        }
    }

    pub fn new_assign(id: usize, name: Token, value: Expr) -> Self {
        Expr::Assign {
            id,
            name,
            value: Box::new(value),
        }
    }

    pub fn new_set(id: usize, object: Box<Expr>, name: Token, value: Expr) -> Self {
        Expr::Set {
            id,
            object,
            name,
            value: Box::new(value),
        }
    }

    pub fn new_function(
        id: usize,
        parameters: Vec<Token>,
        body: Vec<Box<Stmt>>,
        paren: Token,
    ) -> Self {
        Expr::AnonFunction {
            id,
            arguments: parameters,
            body,
            paren,
        }
    }

    pub fn new_binary(id: usize, left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Binary {
            id,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn new_call(id: usize, callee: Expr, paren: Token, arguments: Vec<Expr>) -> Self {
        Expr::Call {
            id,
            callee: Box::new(callee),
            paren,
            arguments,
        }
    }

    pub fn new_get(id: usize, object: Expr, name: Token) -> Self {
        Expr::Get {
            id,
            object: Box::new(object),
            name,
        }
    }

    pub fn new_grouping(id: usize, expression: Expr) -> Self {
        Expr::Grouping {
            id,
            expression: Box::new(expression),
        }
    }

    pub fn new_super(id: usize, keyword: Token, method: Token) -> Self {
        Expr::Super {
            id,
            keyword,
            method,
        }
    }

    pub fn new_this(id: usize, keyword: Token) -> Self {
        Expr::This { id, keyword }
    }

    pub fn new_literal(id: usize, value: LiteralValue) -> Self {
        Expr::Literal { id, value }
    }

    pub fn new_logical(id: usize, left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Logical {
            id,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn new_unary(id: usize, operator: Token, right: Expr) -> Self {
        Expr::Unary {
            id,
            operator,
            right: Box::new(right),
        }
    }

    pub fn new_variable(id: usize, name: Token) -> Self {
        Expr::Variable { id, name }
    }

    pub fn new_type_of(id: usize, expression: Expr) -> Self {
        Expr::TypeOf {
            id,
            expression: Box::new(expression),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Expr::AnonFunction {
                arguments: parameters,
                body,
                ..
            } => {
                let mut s = String::new();
                s.push_str("(fun (");
                for param in parameters {
                    s.push_str(&param.lexeme);
                    s.push_str(", ");
                }
                s.push_str(") ");
                for stmt in body {
                    s.push_str(&stmt.to_string());
                    s.push_str(" ");
                }
                s.push_str(")");
                s
            }
            Expr::Assign { name, value, .. } => {
                format!("(= {} {})", name.lexeme, value.to_string())
            }
            Expr::Binary {
                left,
                operator,
                right,
                ..
            } => {
                format!(
                    "({} {} {})",
                    operator.lexeme,
                    left.to_string(),
                    right.to_string()
                )
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
                ..
            } => {
                let mut s = String::new();
                s.push_str(&callee.to_string());
                s.push_str("(");
                for arg in arguments {
                    s.push_str(&arg.to_string());
                    s.push_str(", ");
                }
                s.push_str(")");
                s
            }
            Expr::Get { object, name, .. } => {
                format!("(get {} {})", object.to_string(), name.lexeme)
            }
            Expr::Grouping { expression, .. } => {
                format!("(group {})", expression.to_string())
            }
            Expr::Literal { value, .. } => match value {
                LiteralValue::Integer(i) => format!("{}", i),
                LiteralValue::Float(f) => format!("{}", f),
                LiteralValue::String(s) => format!("{}", s),
                LiteralValue::Boolean(b) => format!("{}", b),
                LiteralValue::Nil => format!("nil"),
                LiteralValue::Callable(CallableImpl::Function(FunctionImpl { name, .. })) => {
                    format!("<fn {}>", name)
                }
                LiteralValue::Callable(CallableImpl::NativeFunction(NativeFunctionImpl {
                    name,
                    ..
                })) => {
                    format!("<native fn {}>", name)
                }
                LiteralValue::Class { name, .. } => format!("<class {}>", name),
                LiteralValue::Instance {
                    class: class_name, ..
                } => {
                    format!("<instance of {}>", class_name)
                }
            },
            Expr::Logical {
                left,
                operator,
                right,
                ..
            } => {
                format!(
                    "({} {} {})",
                    operator.lexeme,
                    left.to_string(),
                    right.to_string()
                )
            }
            Expr::Set { object, name, .. } => {
                format!("(set {} {})", object.to_string(), name.lexeme)
            }
            Expr::Super {
                keyword, method, ..
            } => {
                format!("(super {} {})", keyword.lexeme, method.lexeme)
            }
            Expr::This { keyword, .. } => format!("(this {})", keyword.lexeme),
            Expr::TypeOf { expression, .. } => {
                format!("(typeof {})", expression.to_string())
            }
            Expr::Unary {
                operator, right, ..
            } => {
                format!("({} {})", operator.lexeme, right.to_string())
            }
            Expr::Variable { name, .. } => format!("(var {})", name.lexeme),
        }
    }

    pub fn evaluate(&self, environment: Environment) -> Result<LiteralValue> {
        match self {
            Expr::AnonFunction {
                arguments, body, ..
            } => {
                let arity = arguments.len();
                let arguments: Vec<Token> = arguments.iter().map(|x| (*x).clone()).collect();
                let body: Vec<Box<Stmt>> = body.iter().map(|x| (*x).clone()).collect();

                Ok(LiteralValue::Callable(CallableImpl::Function(
                    FunctionImpl {
                        name: "<anon>".to_string(),
                        arity,
                        parent_env: environment,
                        params: arguments,
                        body,
                    },
                )))
            }
            Expr::Assign { name, value, .. } => {
                let new_value = value.evaluate(environment.clone())?;

                let assign_success =
                    environment.assign(&name.lexeme, new_value.clone(), self.get_id());

                if assign_success {
                    Ok(new_value)
                } else {
                    bail!("Line {}: Undefined variable '{}'.", name.line, name.lexeme)
                }
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
                ..
            } => {
                let callable = callee.evaluate(environment.clone())?;
                let callable_clone = callable.clone();

                match callable {
                    LiteralValue::Callable(CallableImpl::Function(fun)) => {
                        run_function(fun, environment, arguments)
                    }
                    LiteralValue::Callable(CallableImpl::NativeFunction(NativeFunctionImpl {
                        fun,
                        ..
                    })) => {
                        let mut evaluated_arguments = Vec::new();
                        for arg in arguments {
                            evaluated_arguments.push(arg.evaluate(environment.clone())?);
                        }

                        Ok(fun(&evaluated_arguments))
                    }
                    LiteralValue::Class {
                        methods,
                        attributes,
                        ..
                    } => {
                        let mut fields = HashMap::new();

                        for (name, value) in attributes {
                            fields.insert(name.clone(), value.clone());
                        }

                        let instance = LiteralValue::Instance {
                            class: Box::new(callable_clone.clone()),
                            fields: Rc::new(RefCell::new(fields)),
                        };

                        if let Some(init_method) = methods.get("init") {
                            if init_method.arity != arguments.len() {
                                bail!(
                                    "Expected {} arguments but got {}.",
                                    init_method.arity,
                                    arguments.len()
                                );
                            }

                            let mut init_method = init_method.clone();
                            init_method.parent_env = init_method.parent_env.enclose();
                            init_method
                                .parent_env
                                .define("this".to_string(), instance.clone());

                            run_function(init_method.clone(), environment, arguments)?;
                        }

                        Ok(instance)
                    }
                    _ => bail!("Can only call functions and classes."),
                }
            }
            Expr::Variable { name, .. } => {
                if let Some(value) = environment.get(&name.lexeme, self.get_id()) {
                    Ok(value.clone())
                } else {
                    bail!(
                        "Line {}: Variable '{}' is not defined.",
                        name.line,
                        name.lexeme
                    )
                }
            }
            Expr::Get { object, name, .. } => {
                let object = object.evaluate(environment.clone())?;

                match object.clone() {
                    LiteralValue::Instance { class, fields } => {
                        if let LiteralValue::Class { .. } = &*class {
                            if let Some(value) = fields.borrow().get(&name.lexeme) {
                                Ok(value.clone())
                            } else if let Some(method) = find_method(&name.lexeme, *class.clone()) {
                                let mut callable_impl = method.clone();
                                let new_env = callable_impl.parent_env.enclose();
                                new_env.define("this".to_string(), object.clone());
                                callable_impl.parent_env = new_env;

                                Ok(LiteralValue::Callable(CallableImpl::Function(
                                    callable_impl,
                                )))
                            } else {
                                bail!("Line {}: Undefined property '{}'.", name.line, name.lexeme)
                            }
                        } else {
                            panic!("Expected a class")
                        }
                    }
                    _ => bail!("Only instances have properties."),
                }
            }
            Expr::Grouping { expression, .. } => expression.evaluate(environment),
            Expr::Literal { value, .. } => Ok(value.clone()),
            Expr::Logical {
                left,
                operator,
                right,
                ..
            } => {
                let left = left.evaluate(environment.clone())?;

                match operator.token_type {
                    TokenType::Or => {
                        if left.is_truthy() {
                            Ok(left)
                        } else {
                            right.evaluate(environment.clone())
                        }
                    }
                    TokenType::And => {
                        if left.is_falsey() {
                            Ok(left)
                        } else {
                            right.evaluate(environment)
                        }
                    }
                    _ => bail!("Invalid operator"),
                }
            }
            Expr::Set {
                object,
                name,
                value,
                ..
            } => {
                let object = object.evaluate(environment.clone())?;

                match object {
                    LiteralValue::Instance { class, fields } => {
                        if let LiteralValue::Class { .. } = &*class {
                            let new_value = value.evaluate(environment.clone())?;
                            fields
                                .borrow_mut()
                                .insert(name.lexeme.clone(), new_value.clone());
                            Ok(new_value)
                        } else {
                            panic!("Expected a class")
                        }
                    }
                    _ => bail!("Only instances have fields."),
                }
            }
            Expr::Super { method, id, .. } => {
                let superclass = environment.get("super", self.get_id()).expect(&format!(
                    "Could not find superclass in environment:\n{}",
                    environment.dump(0)
                ));
                let instance = environment
                    .get_this_instance(self.get_id())
                    .expect(&format!(
                        "Tried to get this from non-existent environment at expression {}",
                        id
                    ));

                if let LiteralValue::Class { methods, .. } = superclass {
                    if let Some(method) = methods.get(&method.lexeme) {
                        let mut callable_impl = method.clone();
                        callable_impl.parent_env = callable_impl.parent_env.enclose();
                        callable_impl
                            .parent_env
                            .define("this".to_string(), instance.clone());

                        Ok(LiteralValue::Callable(CallableImpl::Function(
                            callable_impl,
                        )))
                    } else {
                        bail!(
                            "Line {}: Undefined property '{}'.",
                            method.line,
                            method.lexeme
                        )
                    }
                } else {
                    panic!("Expected a class")
                }
            }
            Expr::This { keyword, .. } => {
                if let Some(value) = environment.get(&keyword.lexeme, self.get_id()) {
                    Ok(value.clone())
                } else {
                    bail!(
                        "Line {}: Undefined variable '{}'.",
                        keyword.line,
                        keyword.lexeme
                    )
                }
            }
            Expr::TypeOf { expression, .. } => {
                let value = expression.evaluate(environment.clone())?;
                Ok(LiteralValue::String(value.type_name()))
            }
            Expr::Binary {
                left,
                operator,
                right,
                ..
            } => {
                let left = left.evaluate(environment.clone())?;
                let right = right.evaluate(environment)?;

                match (&left, &operator.token_type, &right) {
                    (LiteralValue::Integer(l), TokenType::Minus, LiteralValue::Integer(r)) => {
                        Ok(LiteralValue::Integer(l - r))
                    }
                    (LiteralValue::Integer(l), TokenType::Plus, LiteralValue::Integer(r)) => {
                        Ok(LiteralValue::Integer(l + r))
                    }
                    (LiteralValue::Integer(l), TokenType::Slash, LiteralValue::Integer(r)) => {
                        Ok(LiteralValue::Integer(l / r))
                    }
                    (LiteralValue::Integer(l), TokenType::Star, LiteralValue::Integer(r)) => {
                        Ok(LiteralValue::Integer(l * r))
                    }
                    (LiteralValue::Integer(l), TokenType::Minus, LiteralValue::Float(r)) => {
                        Ok(LiteralValue::Float(*l as f64 - r))
                    }
                    (LiteralValue::Integer(l), TokenType::Plus, LiteralValue::Float(r)) => {
                        Ok(LiteralValue::Float(*l as f64 + r))
                    }
                    (LiteralValue::Integer(l), TokenType::Slash, LiteralValue::Float(r)) => {
                        Ok(LiteralValue::Float(*l as f64 / r))
                    }
                    (LiteralValue::Integer(l), TokenType::Star, LiteralValue::Float(r)) => {
                        Ok(LiteralValue::Float(*l as f64 * r))
                    }
                    (LiteralValue::Float(l), TokenType::Minus, LiteralValue::Float(r)) => {
                        Ok(LiteralValue::Float(l - r))
                    }
                    (LiteralValue::Float(l), TokenType::Plus, LiteralValue::Float(r)) => {
                        Ok(LiteralValue::Float(l + r))
                    }
                    (LiteralValue::Float(l), TokenType::Slash, LiteralValue::Float(r)) => {
                        Ok(LiteralValue::Float(l / r))
                    }
                    (LiteralValue::Float(l), TokenType::Star, LiteralValue::Float(r)) => {
                        Ok(LiteralValue::Float(l * r))
                    }
                    (LiteralValue::String(l), TokenType::Plus, LiteralValue::String(r)) => {
                        Ok(LiteralValue::String(format!("{}{}", l, r)))
                    }
                    (x, TokenType::BangEqual, y) => Ok(LiteralValue::Boolean(x != y)),
                    (x, TokenType::EqualEqual, y) => Ok(LiteralValue::Boolean(x == y)),
                    (x, TokenType::Less, y) => Ok(LiteralValue::Boolean(x < y)),
                    (x, TokenType::LessEqual, y) => Ok(LiteralValue::Boolean(x <= y)),
                    (x, TokenType::Greater, y) => Ok(LiteralValue::Boolean(x > y)),
                    (x, TokenType::GreaterEqual, y) => Ok(LiteralValue::Boolean(x >= y)),
                    (x, y, z) => bail!(
                        "Invalid operands for operator '{}': {} and {}",
                        y,
                        x.to_string(),
                        z.to_string()
                    ),
                }
            }
            Expr::Unary {
                operator, right, ..
            } => {
                let right = right.evaluate(environment)?;

                match operator.token_type {
                    TokenType::Minus => match right {
                        LiteralValue::Integer(r) => Ok(LiteralValue::Integer(-r)),
                        LiteralValue::Float(r) => Ok(LiteralValue::Float(-r)),
                        _ => bail!("Invalid operand for unary minus"),
                    },
                    TokenType::Bang => match right {
                        LiteralValue::Boolean(r) => Ok(LiteralValue::Boolean(!r)),
                        _ => bail!("Invalid operand for unary bang"),
                    },
                    _ => bail!("Invalid operator"),
                }
            }
        }
    }
}

fn find_method(name: &String, class: LiteralValue) -> Option<FunctionImpl> {
    if let LiteralValue::Class {
        name: _,
        methods,
        superclass,
        attributes: _,
    } = class
    {
        if let Some(method) = methods.get(name) {
            Some(method.clone())
        } else if let Some(superclass) = superclass {
            find_method(name, *superclass)
        } else {
            None
        }
    } else {
        panic!("Expected a class")
    }
}

pub fn run_function(
    fun: FunctionImpl,
    environment: Environment,
    arguments: &Vec<Expr>,
) -> Result<LiteralValue> {
    let FunctionImpl {
        name: _,
        arity,
        parent_env,
        params,
        body,
    } = fun;

    if arguments.len() != arity {
        bail!("Expected {} arguments but got {}.", arity, arguments.len());
    };

    let mut evaluated_arguments = Vec::new();
    for arg in arguments {
        evaluated_arguments.push(arg.evaluate(environment.clone())?);
    }

    let fun_env = parent_env.enclose();

    for (i, param) in params.iter().enumerate() {
        fun_env.define(param.lexeme.clone(), evaluated_arguments[i].clone());
    }

    let mut int = Interpreter::with_environment(fun_env);
    for i in 0..body.len() {
        int.interpret(vec![&body[i]]).expect(&format!(
            "Evaluation failed inside anon function at statement {}",
            i
        ));

        if let Some(return_value) = int.specials.get("return") {
            return Ok(return_value.clone());
        }
    }

    Ok(LiteralValue::Nil)
}

impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Expr::AnonFunction { id, .. } => {
                "anon".hash(state);
                id.hash(state);
            }
            Expr::Assign { id, .. } => {
                "assign".hash(state);
                id.hash(state);
            }
            Expr::Binary { id, .. } => {
                "binary".hash(state);
                id.hash(state);
            }
            Expr::Call { id, .. } => {
                "call".hash(state);
                id.hash(state);
            }
            Expr::Get { id, .. } => {
                "get".hash(state);
                id.hash(state);
            }
            Expr::Grouping { id, .. } => {
                "grouping".hash(state);
                id.hash(state);
            }
            Expr::Literal { id, .. } => {
                "literal".hash(state);
                id.hash(state);
            }
            Expr::Logical { id, .. } => {
                "logical".hash(state);
                id.hash(state);
            }
            Expr::Set { id, .. } => {
                "set".hash(state);
                id.hash(state);
            }
            Expr::Super { id, .. } => {
                "super".hash(state);
                id.hash(state);
            }
            Expr::This { id, .. } => {
                "this".hash(state);
                id.hash(state);
            }
            Expr::TypeOf { id, .. } => {
                "typeof".hash(state);
                id.hash(state);
            }
            Expr::Unary { id, .. } => {
                "unary".hash(state);
                id.hash(state);
            }
            Expr::Variable { id, .. } => {
                "variable".hash(state);
                id.hash(state);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::TokenType;
    use std::collections::HashMap;

    #[test]
    fn expr_is_hashable() {
        // Test 2 times the same expression to make sure that the hash is not based on the pointer
        let expr = Expr::new_binary(
            0,
            Expr::new_unary(
                1,
                Token::new(TokenType::Minus, "-".to_string(), None, 1),
                Expr::new_literal(2, LiteralValue::Integer(123)),
            ),
            Token::new(TokenType::Star, "*".to_string(), None, 1),
            Expr::new_grouping(3, Expr::new_literal(4, LiteralValue::Float(45.67))),
        );

        let expr2 = Expr::new_binary(
            0,
            Expr::new_unary(
                1,
                Token::new(TokenType::Minus, "-".to_string(), None, 1),
                Expr::new_literal(2, LiteralValue::Integer(123)),
            ),
            Token::new(TokenType::Star, "*".to_string(), None, 1),
            Expr::new_grouping(3, Expr::new_literal(4, LiteralValue::Float(45.67))),
        );

        let mut map = HashMap::new();
        map.insert(expr, 1);
        map.insert(expr2, 2);

        assert_eq!(map.len(), 1);
    }

    #[test]
    fn test_expr_to_string() {
        let expr = Expr::new_binary(
            0,
            Expr::new_unary(
                1,
                Token::new(TokenType::Minus, "-".to_string(), None, 1),
                Expr::new_literal(2, LiteralValue::Integer(123)),
            ),
            Token::new(TokenType::Star, "*".to_string(), None, 1),
            Expr::new_grouping(3, Expr::new_literal(4, LiteralValue::Float(45.67))),
        );

        assert_eq!(expr.to_string(), "(* (- 123) (group 45.67))".to_string());
    }
}
