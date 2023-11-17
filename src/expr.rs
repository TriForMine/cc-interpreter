use crate::environment::Environment;
use crate::scanner::{Token, TokenType};
use std::cell::RefCell;
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
        fun: Rc<dyn Fn(Rc<RefCell<Environment>>, &Vec<LiteralValue>) -> LiteralValue>,
    },
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

#[derive(PartialEq)]
pub enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },

    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },

    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },

    Grouping {
        expression: Box<Expr>,
    },

    Literal {
        value: LiteralValue,
    },

    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },

    Unary {
        operator: Token,
        right: Box<Expr>,
    },

    Variable {
        name: Token,
    },
}

impl Expr {
    pub fn new_assign(name: Token, value: Expr) -> Self {
        Expr::Assign {
            name,
            value: Box::new(value),
        }
    }

    pub fn new_binary(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn new_call(callee: Expr, paren: Token, arguments: Vec<Expr>) -> Self {
        Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        }
    }

    pub fn new_grouping(expression: Expr) -> Self {
        Expr::Grouping {
            expression: Box::new(expression),
        }
    }

    pub fn new_literal(value: LiteralValue) -> Self {
        Expr::Literal { value }
    }

    pub fn new_logical(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn new_unary(operator: Token, right: Expr) -> Self {
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    pub fn new_variable(name: Token) -> Self {
        Expr::Variable { name }
    }

    pub fn to_string(&self) -> String {
        match self {
            Expr::Assign { name, value } => {
                format!("(= {} {})", name.lexeme, value.to_string())
            }
            Expr::Binary {
                left,
                operator,
                right,
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
                paren,
                arguments,
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
            Expr::Grouping { expression } => {
                format!("(group {})", expression.to_string())
            }
            Expr::Literal { value } => match value {
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
            } => {
                format!(
                    "({} {} {})",
                    operator.lexeme,
                    left.to_string(),
                    right.to_string()
                )
            }
            Expr::Unary { operator, right } => {
                format!("({} {})", operator.lexeme, right.to_string())
            }
            Expr::Variable { name } => format!("(var {})", name.lexeme),
        }
    }

    pub fn evaluate(
        &self,
        environement: Rc<RefCell<Environment>>,
    ) -> Result<LiteralValue, std::io::Error> {
        match self {
            Expr::Assign { name, value } => {
                let new_value = value.evaluate(environement.clone())?;
                let assign_success = environement
                    .borrow_mut()
                    .assign(&name.lexeme, new_value.clone());

                if assign_success {
                    Ok(new_value)
                } else {
                    Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        format!("Undefined variable '{}'.", name.lexeme),
                    ))
                }
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                let callee = callee.evaluate(environement.clone())?;

                match callee {
                    LiteralValue::Callable { arity, .. } => {
                        if arguments.len() != arity {
                            return Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidInput,
                                format!(
                                    "Expected {} arguments but got {}.",
                                    arity,
                                    arguments.len()
                                ),
                            ));
                        };

                        let mut evaluated_arguments = Vec::new();
                        for arg in arguments {
                            evaluated_arguments.push(arg.evaluate(environement.clone())?);
                        }

                        let fun = match callee {
                            LiteralValue::Callable { fun, .. } => fun,
                            _ => panic!("Expected a function"),
                        };

                        Ok(fun(environement.clone(), &evaluated_arguments))
                    }
                    _ => Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "Can only call functions and classes.",
                    )),
                }
            }
            Expr::Variable { name } => {
                if let Some(value) = environement.borrow().get(&name.lexeme) {
                    Ok(value.clone())
                } else {
                    Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        format!("Undefined variable '{}'.", name.lexeme),
                    ))
                }
            }
            Expr::Grouping { expression } => expression.evaluate(environement),
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate(environement.clone())?;

                match operator.token_type {
                    TokenType::Or => {
                        if left.is_truthy() {
                            Ok(left)
                        } else {
                            right.evaluate(environement.clone())
                        }
                    }
                    TokenType::And => {
                        if left.is_falsey() {
                            Ok(left)
                        } else {
                            right.evaluate(environement)
                        }
                    }
                    _ => Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "Invalid operator",
                    )),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate(environement.clone())?;
                let right = right.evaluate(environement)?;

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
                    (x, y, z) => Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        format!("Invalid operands for binary operator: {} {} {}", x, y, z),
                    )),
                }
            }
            Expr::Unary { operator, right } => {
                let right = right.evaluate(environement)?;

                match operator.token_type {
                    TokenType::Minus => match right {
                        LiteralValue::Integer(r) => Ok(LiteralValue::Integer(-r)),
                        LiteralValue::Float(r) => Ok(LiteralValue::Float(-r)),
                        _ => Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidInput,
                            "Invalid operand for unary minus",
                        )),
                    },
                    TokenType::Bang => match right {
                        LiteralValue::Boolean(r) => Ok(LiteralValue::Boolean(!r)),
                        _ => Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidInput,
                            "Invalid operand for unary bang",
                        )),
                    },
                    _ => Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "Invalid operator",
                    )),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::TokenType;

    #[test]
    fn test_expr_to_string() {
        let expr = Expr::new_binary(
            Expr::new_unary(
                Token::new(TokenType::Minus, "-".to_string(), None, 1),
                Expr::new_literal(LiteralValue::Integer(123)),
            ),
            Token::new(TokenType::Star, "*".to_string(), None, 1),
            Expr::new_grouping(Expr::new_literal(LiteralValue::Float(45.67))),
        );

        assert_eq!(expr.to_string(), "(* (- 123) (group 45.67))".to_string());
    }
}
