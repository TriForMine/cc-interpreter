use crate::expr::{Expr, LiteralValue};

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn interpret(&mut self, expr: Expr) -> Result<LiteralValue, std::io::Error> {
        expr.evaluate()
    }
}
