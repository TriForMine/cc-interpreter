use crate::expr::Expr;
use crate::scanner::Token;
use std::fmt::Display;
use std::hash::Hash;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Stmt {
    Block {
        statements: Vec<Box<Stmt>>,
    },
    Expression {
        expression: Expr,
    },
    Print {
        expression: Expr,
    },
    Var {
        name: Token,
        initializer: Expr,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Box<Stmt>>,
    },
    Return {
        keyword: Token,
        value: Option<Expr>,
    },
}

impl Stmt {
    pub fn to_string(&self) -> String {
        match self {
            Stmt::Block { statements } => {
                let mut s = String::new();
                for stmt in statements {
                    s.push_str(&stmt.to_string());
                }
                s
            }
            Stmt::Expression { expression } => expression.to_string(),
            Stmt::Print { expression } => {
                let mut s = String::new();
                s.push_str("print ");
                s.push_str(&expression.to_string());
                s.push_str(";");
                s
            }
            Stmt::Var { name, initializer } => {
                let mut s = String::new();
                s.push_str(&name.lexeme);
                s.push_str(" = ");
                s.push_str(&initializer.to_string());
                s.push_str(";");
                s
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut s = String::new();
                s.push_str("if (");
                s.push_str(&condition.to_string());
                s.push_str(") ");
                s.push_str(&then_branch.to_string());
                if let Some(else_branch) = else_branch {
                    s.push_str(" else ");
                    s.push_str(&else_branch.to_string());
                }
                s
            }
            Stmt::Return { keyword, value } => {
                let mut s = String::new();
                s.push_str(&keyword.lexeme);
                if let Some(value) = value {
                    s.push_str(" ");
                    s.push_str(&value.to_string());
                }
                s.push_str(";");
                s
            }
            Stmt::While { condition, body } => {
                let mut s = String::new();
                s.push_str("while (");
                s.push_str(&condition.to_string());
                s.push_str(") ");
                s.push_str(&body.to_string());
                s
            }
            Stmt::Function { name, params, body } => {
                let mut s = String::new();
                s.push_str("fun ");
                s.push_str(&name.lexeme);
                s.push_str("(");
                for param in params {
                    s.push_str(&param.lexeme);
                    s.push_str(", ");
                }
                s.push_str(") ");
                for stmt in body {
                    s.push_str(&stmt.to_string());
                }
                s
            }
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Hash for Stmt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}
