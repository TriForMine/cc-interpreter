use crate::scanner::Token;

pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },

    Grouping {
        expression: Box<Expr>,
    },

    Literal {
        value: Token,
    },

    Unary {
        operator: Token,
        right: Box<Expr>,
    },
}
