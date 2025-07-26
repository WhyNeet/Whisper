use std::{cell::RefCell, collections::HashMap, rc::Rc};

use tcast::types::Type;

use crate::resolver::TypeResolver;

#[derive(Clone, Default)]
pub struct Scope {
    enclosing: Option<Rc<Scope>>,
    values: RefCell<HashMap<String, Type>>,
    resolver: Rc<TypeResolver>,
    namespaces: Rc<RefCell<HashMap<String, Rc<Scope>>>>,
}

impl Scope {
    pub fn new(enclosing: Rc<Scope>) -> Rc<Self> {
        let scope = Rc::new(Self {
            enclosing: Some(Rc::clone(&enclosing)),
            resolver: Rc::new(TypeResolver::new(Rc::clone(&enclosing.resolver))),
            namespaces: Default::default(),
            values: Default::default(),
        });

        scope
    }

    pub fn root() -> Self {
        Self {
            resolver: Rc::new(TypeResolver::root()),
            namespaces: Default::default(),
            values: Default::default(),
            enclosing: None,
        }
    }

    pub fn insert(&self, ident: String, ty: Type) -> bool {
        self.values.borrow_mut().insert(ident, ty).is_some()
    }

    pub fn get(&self, ident: &str) -> Option<Type> {
        self.values
            .borrow()
            .get(ident)
            .cloned()
            .or_else(|| {
                self.namespaces
                    .borrow()
                    .get(ident)
                    .map(|ns| Type::Namespace {
                        alias: ident.to_string(),
                        fields: ns
                            .values
                            .borrow()
                            .iter()
                            .map(|(name, ty)| (name.clone(), ty.clone()))
                            .collect(),
                    })
            })
            .or_else(|| {
                self.enclosing
                    .as_ref()
                    .map(|scope| scope.get(ident))
                    .unwrap_or(None)
            })
    }

    pub fn create_namespace(&self, name: String, scope: Rc<Scope>) -> Option<Rc<Scope>> {
        let scope = Scope::new(scope);

        if self
            .namespaces
            .borrow_mut()
            .insert(name, Rc::clone(&scope))
            .is_some()
        {
            None
        } else {
            Some(scope)
        }
    }

    pub fn type_resolver(&self) -> &TypeResolver {
        &self.resolver
    }
}
