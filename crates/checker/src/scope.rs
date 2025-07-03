use std::{cell::RefCell, collections::HashMap, rc::Rc};

use common::types::Type;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    enclosing: Option<Rc<Scope>>,
    values: RefCell<HashMap<String, Type>>,
}

impl Scope {
    pub fn new(enclosing: Rc<Scope>) -> Self {
        Self {
            enclosing: Some(enclosing),
            ..Default::default()
        }
    }

    pub fn insert(&self, ident: String, ty: Type) -> bool {
        self.values.borrow_mut().insert(ident, ty).is_some()
    }

    pub fn get(&self, ident: &str) -> Option<Type> {
        self.values.borrow().get(ident).cloned().or_else(|| {
            self.enclosing
                .as_ref()
                .map(|scope| scope.get(ident))
                .unwrap_or(None)
        })
    }
}
