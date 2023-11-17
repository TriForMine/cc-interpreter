use crate::expr::LiteralValue;
use std::collections::HashMap;

pub struct Environment<'a> {
    values: HashMap<&'a str, LiteralValue<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &'a str, value: LiteralValue<'a>) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&LiteralValue> {
        self.values.get(name)
    }
}
