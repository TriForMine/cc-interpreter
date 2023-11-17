use crate::expr::Expr;
use crate::scanner::Token;

pub enum Stmt<'a> {
    Expression {
        expression: &'a Expr<'a>,
    },
    Print {
        expression: &'a Expr<'a>,
    },
    Var {
        name: &'a Token<'a>,
        initializer: &'a Expr<'a>,
    },
}
