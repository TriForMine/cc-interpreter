use crate::expr::{CallableImpl, LiteralValue, NativeFunctionImpl};
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    values: Rc<RefCell<HashMap<String, LiteralValue>>>,
    locals: Rc<RefCell<HashMap<usize, usize>>>,
    pub enclosing: Option<Box<Environment>>,
}

fn clock_impl(_args: &Vec<LiteralValue>) -> LiteralValue {
    LiteralValue::Float(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}

fn get_globals() -> Rc<RefCell<HashMap<String, LiteralValue>>> {
    let mut globals = HashMap::new();
    let callable_impl = NativeFunctionImpl {
        name: "clock".to_string(),
        arity: 0,
        fun: Rc::new(clock_impl),
    };

    globals.insert(
        "clock".to_string(),
        LiteralValue::Callable(CallableImpl::NativeFunction(callable_impl)),
    );
    Rc::new(RefCell::new(globals))
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: get_globals(),
            locals: Rc::new(RefCell::new(HashMap::new())),
            enclosing: None,
        }
    }

    pub fn enclose(&self) -> Environment {
        Environment {
            values: get_globals(),
            locals: self.locals.clone(),
            enclosing: Some(Box::new(self.clone())),
        }
    }

    pub fn resolve(&mut self, locals: HashMap<usize, usize>) {
        for (child, parent) in locals {
            self.locals.borrow_mut().insert(child, parent);
        }
    }

    pub fn define(&self, name: String, value: LiteralValue) {
        self.values.borrow_mut().insert(name, value);
    }

    pub fn get(&self, name: &str, expr_id: usize) -> Option<LiteralValue> {
        let distance = self.locals.borrow().get(&expr_id).cloned();
        self.get_internal(name, distance)
    }

    pub fn get_this_instance(&self, super_id: usize) -> Option<LiteralValue> {
        let distance = self
            .locals
            .borrow()
            .get(&super_id)
            .cloned()
            .expect("Tried to get this from non-existent environment");
        self.get_internal("this", Some(distance - 1))
    }

    pub fn get_internal(&self, name: &str, distance: Option<usize>) -> Option<LiteralValue> {
        if let Some(distance) = distance {
            if distance == 0 {
                self.values.borrow().get(name).cloned()
            } else {
                match &self.enclosing {
                    None => panic!("Tried to get variable from non-existent environment"),
                    Some(enclosing) => enclosing.get_internal(name, Some(distance - 1)),
                }
            }
        } else {
            match &self.enclosing {
                None => self.values.borrow().get(name).cloned(),
                Some(enclosing) => enclosing.get_internal(name, distance),
            }
        }
    }

    pub fn assign_global(&self, name: &str, value: LiteralValue) -> bool {
        self.assign_internal(name, value, None)
    }

    pub fn assign(&self, name: &str, value: LiteralValue, expr_id: usize) -> bool {
        let distance = self.locals.borrow().get(&expr_id).cloned();
        self.assign_internal(name, value, distance)
    }

    pub fn assign_internal(
        &self,
        name: &str,
        value: LiteralValue,
        distance: Option<usize>,
    ) -> bool {
        if let Some(distance) = distance {
            if distance == 0 {
                match self.values.borrow_mut().get_mut(name) {
                    None => false,
                    Some(val) => {
                        *val = value;
                        true
                    }
                }
            } else {
                match &self.enclosing {
                    None => panic!("Tried to assign variable in non-existent environment"),
                    Some(enclosing) => enclosing.assign_internal(name, value, Some(distance - 1)),
                }
            }
        } else {
            match &self.enclosing {
                None => match self.values.borrow_mut().get_mut(name) {
                    None => false,
                    Some(val) => {
                        *val = value;
                        true
                    }
                },
                Some(enclosing) => enclosing.assign_internal(name, value, distance),
            }
        }
    }

    #[allow(dead_code)]
    pub fn dump(&self, indent: usize) -> String {
        let mut result = String::new();

        for _ in 0..indent {
            result.push_str(" ");
        }

        result.push_str("Environment {\n");

        for (key, value) in self.values.borrow().iter() {
            for _ in 0..indent + 2 {
                result.push_str(" ");
            }
            result.push_str(&format!("{}: {:?}\n", key, value));
        }

        for (key, value) in self.locals.borrow().iter() {
            for _ in 0..indent + 2 {
                result.push_str(" ");
            }
            result.push_str(&format!("{}: {}\n", key, value));
        }

        if let Some(enclosing) = &self.enclosing {
            for _ in 0..indent + 2 {
                result.push_str(" ");
            }

            result.push_str("enclosing: \n");
            result.push_str(&enclosing.dump(indent + 4));
        }

        for _ in 0..indent {
            result.push_str(" ");
        }

        result.push_str("}\n");

        result
    }
}

impl Hash for Environment {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (key, value) in self.values.borrow().iter() {
            key.hash(state);
            value.hash(state);
        }
        for (key, value) in self.locals.borrow().iter() {
            key.hash(state);
            value.hash(state);
        }
        self.enclosing.hash(state);
    }
}
