use crate::expr::Expr;
use crate::scanner::Token;
use std::fmt::Display;

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
    IfStmt {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    WhileStmt {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Box<Stmt>>,
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
            Stmt::Print { expression } => expression.to_string(),
            Stmt::Var { name, initializer } => {
                let mut s = String::new();
                s.push_str(&name.lexeme);
                s.push_str(" = ");
                s.push_str(&initializer.to_string());
                s.push_str(";");
                s
            }
            Stmt::IfStmt {
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
            Stmt::WhileStmt { condition, body } => {
                let mut s = String::new();
                s.push_str("while (");
                s.push_str(&condition.to_string());
                s.push_str(") ");
                s.push_str(&body.to_string());
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
