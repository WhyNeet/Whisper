use std::{cell::RefCell, collections::HashMap, rc::Rc};

use common::effects::Effect;
use tcast::types::Type;

use crate::resolver::TypeResolver;

#[derive(Debug, Clone, Default)]
pub struct Scope {
    id: u64,
    enclosing: Option<Rc<Scope>>,
    values: RefCell<HashMap<String, Type>>,
    resolver: Rc<TypeResolver>,
    namespaces: RefCell<HashMap<String, Rc<Scope>>>,
    descendants: Rc<RefCell<Vec<Rc<Scope>>>>,
}

impl Scope {
    pub fn js_default() -> Self {
        let mut values = HashMap::new();

        values.insert(
            "console".to_string(),
            Type::Struct {
                fields: vec![(
                    "log".to_string(),
                    Type::Fn {
                        return_type: Box::new(Type::Unit),
                        params: vec![Type::String],
                        effects: vec![Effect::Io],
                    },
                )],
                alias: "Console".to_string(),
            },
        );

        Self {
            id: 0,
            values: RefCell::new(values),
            ..Default::default()
        }
    }

    pub fn id(&self) -> u64 {
        self.id
    }
}

impl Scope {
    pub fn new(enclosing: Rc<Scope>) -> Rc<Self> {
        let mut desc = enclosing.descendants.borrow_mut();
        let id = desc.len();
        let scope = Rc::new(Self {
            enclosing: Some(Rc::clone(&enclosing)),
            id: id as u64,
            resolver: Rc::new(TypeResolver::new(Rc::clone(&enclosing.resolver))),
            ..Scope::js_default()
        });

        desc.push(Rc::clone(&scope));

        scope
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

    pub fn create_namespace(&self, name: String) -> Option<Rc<Scope>> {
        let ns = Rc::new(Scope::js_default());

        if self
            .namespaces
            .borrow_mut()
            .insert(name, Rc::clone(&ns))
            .is_some()
        {
            None
        } else {
            Some(ns)
        }
    }

    pub fn type_resolver(&self) -> &TypeResolver {
        &self.resolver
    }
}
