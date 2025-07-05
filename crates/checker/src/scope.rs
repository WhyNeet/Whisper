use std::{cell::RefCell, collections::HashMap, rc::Rc};

use common::{effects::Effect, types::Type};

#[derive(Debug, Clone)]
pub struct Scope {
    enclosing: Option<Rc<Scope>>,
    values: RefCell<HashMap<String, Type>>,
}

impl Scope {
    pub fn js_default() -> Self {
        let mut values = HashMap::new();

        values.insert(
            "console".to_string(),
            Type::StructInstance {
                of: Box::new(Type::Struct {
                    fields: vec![(
                        "log".to_string(),
                        Type::Fn {
                            return_type: Box::new(Type::Unit),
                            params: vec![Type::String],
                            effects: vec![Effect::Io],
                        },
                    )],
                }),
            },
        );

        Self {
            enclosing: Default::default(),
            values: RefCell::new(values),
        }
    }
}

impl Scope {
    pub fn new(enclosing: Rc<Scope>) -> Self {
        Self {
            enclosing: Some(enclosing),
            ..Scope::js_default()
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
