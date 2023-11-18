use crate::expr::LiteralValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    values: HashMap<String, LiteralValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

fn clock_impl(_args: &Vec<LiteralValue>) -> LiteralValue {
    LiteralValue::Float(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}

fn get_globals() -> HashMap<String, LiteralValue> {
    let mut globals = HashMap::new();
    globals.insert(
        "clock".to_string(),
        LiteralValue::Callable {
            name: "clock".to_string(),
            arity: 0,
            fun: Rc::new(clock_impl),
        },
    );
    globals
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: get_globals(),
            enclosing: None,
        }
    }

    pub fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: get_globals(),
            enclosing: Some(enclosing.clone()),
        }
    }

    pub fn define(&mut self, name: String, value: LiteralValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str, distance: Option<usize>) -> Option<LiteralValue> {
        if let Some(distance) = distance {
            if distance == 0 {
                self.values.get(name).cloned()
            } else {
                match &self.enclosing {
                    None => panic!("Tried to get variable from non-existent environment"),
                    Some(enclosing) => enclosing.borrow().get(name, Some(distance - 1)),
                }
            }
        } else {
            match &self.enclosing {
                None => self.values.get(name).cloned(),
                Some(enclosing) => enclosing.borrow().get(name, distance),
            }
        }
    }

    pub fn assign(&mut self, name: &str, value: LiteralValue, distance: Option<usize>) -> bool {
        if let Some(distance) = distance {
            if distance == 0 {
                match self.values.get_mut(name) {
                    None => false,
                    Some(val) => {
                        *val = value;
                        true
                    }
                }
            } else {
                match &self.enclosing {
                    None => panic!("Tried to assign variable in non-existent environment"),
                    Some(enclosing) => {
                        enclosing
                            .borrow_mut()
                            .assign(name, value, Some(distance - 1))
                    }
                }
            }
        } else {
            match &self.enclosing {
                None => match self.values.get_mut(name) {
                    None => false,
                    Some(val) => {
                        *val = value;
                        true
                    }
                },
                Some(enclosing) => enclosing.borrow_mut().assign(name, value, distance),
            }
        }
    }
}
