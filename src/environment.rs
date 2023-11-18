use crate::expr::LiteralValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    values: HashMap<String, LiteralValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: String, value: LiteralValue) {
        self.values.insert(name, value);
    }

    pub fn define_top_level(&mut self, name: String, value: LiteralValue) {
        if let Some(env) = &mut self.enclosing {
            env.borrow_mut().define_top_level(name, value);
        } else {
            self.define(name, value);
        }
    }

    pub fn get(&self, name: &str) -> Option<LiteralValue> {
        let value = self.values.get(name);

        match (value, &self.enclosing) {
            (Some(_), _) => value.cloned(),
            (None, Some(env)) => env.borrow().get(name),
            (None, None) => None,
        }
    }

    pub fn assign(&mut self, name: &str, value: LiteralValue) -> bool {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            true
        } else if let Some(env) = &mut self.enclosing {
            env.borrow_mut().assign(name, value)
        } else {
            false
        }
    }
}
