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

#[derive(Clone)]
pub enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
    Callable {
        name: String,
        arity: usize,
        fun: Rc<dyn Fn(&Vec<LiteralValue>) -> LiteralValue>,
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
            LiteralValue::Callable { name, arity, .. } => {
                "callable".hash(state);
                name.hash(state);
                arity.hash(state);
            }
        }
    }
}

impl Debug for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LiteralValue::Integer(i) => write!(f, "{}", i),
            LiteralValue::Float(fl) => write!(f, "{}", fl),
            LiteralValue::String(s) => write!(f, "{}", s),
            LiteralValue::Boolean(b) => write!(f, "{}", b),
            LiteralValue::Nil => write!(f, "nil"),
            LiteralValue::Callable { name, .. } => write!(f, "<fn {}>", name),
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
            LiteralValue::Callable { name, .. } => format!("<fn {}>", name),
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
        }
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsey()
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
        parameters: Vec<Token>,
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
        }
    }

    pub fn new_assign(id: usize, name: Token, value: Expr) -> Self {
        Expr::Assign {
            id,
            name,
            value: Box::new(value),
        }
    }

    pub fn new_function(id: usize, parameters: Vec<Token>, body: Vec<Box<Stmt>>) -> Self {
        Expr::AnonFunction {
            id,
            parameters,
            body,
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

    pub fn new_grouping(id: usize, expression: Expr) -> Self {
        Expr::Grouping {
            id,
            expression: Box::new(expression),
        }
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

    pub fn to_string(&self) -> String {
        match self {
            Expr::AnonFunction {
                parameters, body, ..
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
            Expr::Grouping { expression, .. } => {
                format!("(group {})", expression.to_string())
            }
            Expr::Literal { value, .. } => match value {
                LiteralValue::Integer(i) => format!("{}", i),
                LiteralValue::Float(f) => format!("{}", f),
                LiteralValue::String(s) => format!("{}", s),
                LiteralValue::Boolean(b) => format!("{}", b),
                LiteralValue::Nil => format!("nil"),
                LiteralValue::Callable { name, .. } => format!("<fn {}>", name),
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
            Expr::Unary {
                operator, right, ..
            } => {
                format!("({} {})", operator.lexeme, right.to_string())
            }
            Expr::Variable { name, .. } => format!("(var {})", name.lexeme),
        }
    }

    pub fn evaluate(
        &self,
        environement: Rc<RefCell<Environment>>,
        locals: Rc<RefCell<HashMap<usize, usize>>>,
    ) -> Result<LiteralValue> {
        match self {
            Expr::AnonFunction {
                parameters, body, ..
            } => {
                let arity = parameters.len();
                let env = environement.clone();
                let locals = locals.clone();

                let parameters: Vec<Token> = parameters.iter().map(|x| (*x).clone()).collect();
                let body: Vec<Box<Stmt>> = body.iter().map(|x| (*x).clone()).collect();

                let fun_impl = move |args: &Vec<LiteralValue>| {
                    let mut anon_env = Interpreter::for_anonymous(env.clone(), locals.clone());

                    for (i, arg) in args.iter().enumerate() {
                        anon_env
                            .environment
                            .borrow_mut()
                            .define(parameters[i].lexeme.clone(), (*arg).clone());
                    }

                    for i in 0..body.len() {
                        anon_env
                            .interpret(vec![&body[i]])
                            .expect("Error in function body");

                        if let Some(return_value) = anon_env.specials.borrow().get("return") {
                            return return_value.clone();
                        }
                    }

                    LiteralValue::Nil
                };

                Ok(LiteralValue::Callable {
                    name: "anon".to_string(),
                    arity,
                    fun: Rc::new(fun_impl),
                })
            }
            Expr::Assign { name, value, .. } => {
                let distance = locals.borrow().get(&self.get_id()).cloned();

                let new_value = value.evaluate(environement.clone(), locals.clone())?;

                let assign_success =
                    environement
                        .borrow_mut()
                        .assign(&name.lexeme, new_value.clone(), distance);

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
                let callee = callee.evaluate(environement.clone(), locals.clone())?;

                match callee {
                    LiteralValue::Callable { arity, .. } => {
                        if arguments.len() != arity {
                            bail!("Expected {} arguments but got {}.", arity, arguments.len());
                        };

                        let mut evaluated_arguments = Vec::new();
                        for arg in arguments {
                            evaluated_arguments
                                .push(arg.evaluate(environement.clone(), locals.clone())?);
                        }

                        let fun = match callee {
                            LiteralValue::Callable { fun, .. } => fun,
                            _ => panic!("Expected a function"),
                        };

                        Ok(fun(&evaluated_arguments))
                    }
                    _ => bail!("Can only call functions and classes."),
                }
            }
            Expr::Variable { name, .. } => {
                let distance = locals.borrow().get(&self.get_id()).cloned();
                if let Some(value) = environement.borrow().get(&name.lexeme, distance) {
                    Ok(value.clone())
                } else {
                    bail!("Line {}: Undefined variable '{}'.", name.line, name.lexeme)
                }
            }
            Expr::Grouping { expression, .. } => expression.evaluate(environement, locals.clone()),

            Expr::Literal { value, .. } => Ok(value.clone()),
            Expr::Logical {
                left,
                operator,
                right,
                ..
            } => {
                let left = left.evaluate(environement.clone(), locals.clone())?;

                match operator.token_type {
                    TokenType::Or => {
                        if left.is_truthy() {
                            Ok(left)
                        } else {
                            right.evaluate(environement.clone(), locals.clone())
                        }
                    }
                    TokenType::And => {
                        if left.is_falsey() {
                            Ok(left)
                        } else {
                            right.evaluate(environement, locals.clone())
                        }
                    }
                    _ => bail!("Invalid operator"),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
                ..
            } => {
                let left = left.evaluate(environement.clone(), locals.clone())?;
                let right = right.evaluate(environement, locals.clone())?;

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
                let right = right.evaluate(environement, locals.clone())?;

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
