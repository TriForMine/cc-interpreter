use crate::environment::Environment;
use crate::scanner::{Token, TokenType};
use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum LiteralValue<'a> {
    Integer(i64),
    Float(f64),
    String(Cow<'a, str>),
    Boolean(bool),
    Nil,
}

impl<'a> LiteralValue<'_> {
    pub fn to_string(&self) -> String {
        format!("{}", self)
    }
}

impl std::fmt::Display for LiteralValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LiteralValue::Integer(i) => write!(f, "{}", i),
            LiteralValue::Float(fl) => write!(f, "{}", fl),
            LiteralValue::String(s) => write!(f, "{}", s),
            LiteralValue::Boolean(b) => write!(f, "{}", b),
            LiteralValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(PartialEq)]
pub enum Expr<'a> {
    Assign {
        name: &'a Token<'a>,
        value: &'a Expr<'a>,
    },

    Binary {
        left: &'a Expr<'a>,
        operator: &'a Token<'a>,
        right: &'a Expr<'a>,
    },

    Grouping {
        expression: &'a Expr<'a>,
    },

    Literal {
        value: &'a LiteralValue<'a>,
    },

    Unary {
        operator: &'a Token<'a>,
        right: &'a Expr<'a>,
    },

    Variable {
        name: &'a Token<'a>,
    },
}

impl<'a> Expr<'a> {
    pub fn new_assign(name: &'a Token<'_>, value: &'a Expr<'a>) -> Self {
        Expr::Assign { name, value }
    }

    pub fn new_binary(left: &'a Expr<'a>, operator: &'a Token<'a>, right: &'a Expr<'a>) -> Self {
        Expr::Binary {
            left,
            operator,
            right,
        }
    }

    pub fn new_grouping(expression: &'a Expr<'a>) -> Self {
        Expr::Grouping { expression }
    }

    pub fn new_literal(value: &'a LiteralValue<'a>) -> Self {
        Expr::Literal { value }
    }

    pub fn new_unary(operator: &'a Token<'a>, right: &'a Expr<'a>) -> Self {
        Expr::Unary { operator, right }
    }

    pub fn new_variable(name: &'a Token<'a>) -> Self {
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
            Expr::Grouping { expression } => {
                format!("(group {})", expression.to_string())
            }
            Expr::Literal { value } => match value {
                LiteralValue::Integer(i) => format!("{}", i),
                LiteralValue::Float(f) => format!("{}", f),
                LiteralValue::String(s) => format!("{}", s),
                LiteralValue::Boolean(b) => format!("{}", b),
                LiteralValue::Nil => format!("nil"),
            },
            Expr::Unary { operator, right } => {
                format!("({} {})", operator.lexeme, right.to_string())
            }
            Expr::Variable { name } => format!("(var {})", name.lexeme),
        }
    }

    pub fn evaluate<'b>(
        &'b self,
        environement: &'b mut Environment<'b>,
    ) -> Result<&LiteralValue, std::io::Error> {
        match self {
            Expr::Assign { name, value } => match environement.get(&name.lexeme) {
                Some(_) => {
                    let value = value.evaluate(environement)?;
                    environement.define(name.lexeme, value.clone());
                    Ok(value)
                }
                None => Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("Undefined variable '{}'.", name.lexeme),
                )),
            },
            Expr::Variable { name } => {
                if let Some(value) = environement.get(&name.lexeme) {
                    Ok(value)
                } else {
                    Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        format!("Undefined variable '{}'.", name.lexeme),
                    ))
                }
            }
            Expr::Grouping { expression } => expression.evaluate(environement),
            Expr::Literal { value } => Ok(value),
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate(environement)?;
                let right = right.evaluate(environement)?;

                match (&left, &operator.token_type, &right) {
                    (LiteralValue::Integer(l), TokenType::Minus, LiteralValue::Integer(r)) => {
                        Ok(&LiteralValue::Integer(l - r))
                    }
                    (LiteralValue::Integer(l), TokenType::Plus, LiteralValue::Integer(r)) => {
                        Ok(&LiteralValue::Integer(l + r))
                    }
                    (LiteralValue::Integer(l), TokenType::Slash, LiteralValue::Integer(r)) => {
                        Ok(&LiteralValue::Integer(l / r))
                    }
                    (LiteralValue::Integer(l), TokenType::Star, LiteralValue::Integer(r)) => {
                        Ok(&LiteralValue::Integer(l * r))
                    }
                    (LiteralValue::Integer(l), TokenType::Minus, LiteralValue::Float(r)) => {
                        Ok(&LiteralValue::Float(*l as f64 - r))
                    }
                    (LiteralValue::Integer(l), TokenType::Plus, LiteralValue::Float(r)) => {
                        Ok(&LiteralValue::Float(*l as f64 + r))
                    }
                    (LiteralValue::Integer(l), TokenType::Slash, LiteralValue::Float(r)) => {
                        Ok(&LiteralValue::Float(*l as f64 / r))
                    }
                    (LiteralValue::Integer(l), TokenType::Star, LiteralValue::Float(r)) => {
                        Ok(&LiteralValue::Float(*l as f64 * r))
                    }
                    (LiteralValue::Float(l), TokenType::Minus, LiteralValue::Float(r)) => {
                        Ok(&LiteralValue::Float(l - r))
                    }
                    (LiteralValue::Float(l), TokenType::Plus, LiteralValue::Float(r)) => {
                        Ok(&LiteralValue::Float(l + r))
                    }
                    (LiteralValue::Float(l), TokenType::Slash, LiteralValue::Float(r)) => {
                        Ok(&LiteralValue::Float(l / r))
                    }
                    (LiteralValue::Float(l), TokenType::Star, LiteralValue::Float(r)) => {
                        Ok(&LiteralValue::Float(l * r))
                    }
                    (LiteralValue::String(l), TokenType::Plus, LiteralValue::String(r)) => {
                        Ok(&LiteralValue::String(Cow::Owned(format!("{}{}", l, r))))
                    }
                    (x, TokenType::BangEqual, y) => Ok(&LiteralValue::Boolean(x != y)),
                    (x, TokenType::EqualEqual, y) => Ok(&LiteralValue::Boolean(x == y)),
                    (x, TokenType::Less, y) => Ok(&LiteralValue::Boolean(x < y)),
                    (x, TokenType::LessEqual, y) => Ok(&LiteralValue::Boolean(x <= y)),
                    (x, TokenType::Greater, y) => Ok(&LiteralValue::Boolean(x > y)),
                    (x, TokenType::GreaterEqual, y) => Ok(&LiteralValue::Boolean(x >= y)),
                    (x, y, z) => Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        format!(
                            "Invalid operands for binary operator: {:?} {:?} {:?}",
                            x, y, z
                        ),
                    )),
                }
            }
            Expr::Unary { operator, right } => {
                let right = right.evaluate(environement)?;

                match operator.token_type {
                    TokenType::Minus => match right {
                        LiteralValue::Integer(r) => Ok(&LiteralValue::Integer(-*r)),
                        LiteralValue::Float(r) => Ok(&LiteralValue::Float(-*r)),
                        _ => Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidInput,
                            "Invalid operand for unary minus",
                        )),
                    },
                    TokenType::Bang => match right {
                        LiteralValue::Boolean(r) => Ok(&LiteralValue::Boolean(!*r)),
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

    pub fn print(&self) {
        println!("{}", self.to_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::TokenType;

    #[test]
    fn test_expr_to_string() {
        let expr = Expr::new_binary(
            Box::new(Expr::new_unary(
                Token::new(TokenType::Minus, "-", None, 1),
                Box::new(Expr::new_literal(LiteralValue::Integer(123))),
            )),
            Token::new(TokenType::Star, "*", None, 1),
            Box::new(Expr::new_grouping(Box::new(Expr::new_literal(
                LiteralValue::Float(45.67),
            )))),
        );

        assert_eq!(expr.to_string(), "(* (- 123) (group 45.67))".to_string());
    }
}
