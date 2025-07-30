use common::types::Type;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use string_cache::DefaultAtom as Atom;
use tcast::namespace::Namespace;

use crate::resolver::TypeResolver;

#[derive(Debug, Clone, Default)]
pub struct Scope {
    enclosing: Option<Rc<Scope>>,
    values: RefCell<HashMap<Atom, ScopeValueData>>,
    resolver: Rc<TypeResolver>,
    namespaces: Rc<RefCell<HashMap<Atom, Rc<Namespace>>>>,
}

impl Scope {
    pub fn new(enclosing: Rc<Scope>) -> Rc<Self> {
        Rc::new(Self {
            enclosing: Some(Rc::clone(&enclosing)),
            resolver: Rc::new(TypeResolver::new(Rc::clone(&enclosing.resolver))),
            namespaces: Default::default(),
            values: Default::default(),
        })
    }

    pub fn root() -> Self {
        Self {
            resolver: Rc::new(TypeResolver::root()),
            namespaces: Default::default(),
            values: Default::default(),
            enclosing: None,
        }
    }

    pub fn is_root(&self) -> bool {
        self.enclosing.is_none()
    }

    pub fn insert(&self, ident: Atom, ty: Type, is_mut: bool) -> bool {
        self.values
            .borrow_mut()
            .insert(ident, ScopeValueData { ty, is_mut })
            .is_some()
    }

    pub fn get(&self, ident: &Atom) -> Option<ScopeValueData> {
        self.values.borrow().get(ident).cloned().or_else(|| {
            self.enclosing
                .as_ref()
                .map(|scope| scope.get(ident))
                .unwrap_or(None)
        })
    }

    pub fn get_namespace(&self, name: &Atom) -> Option<Rc<Namespace>> {
        self.namespaces.borrow().get(name).cloned().or_else(|| {
            self.enclosing
                .as_ref()
                .and_then(|scope| scope.get_namespace(name))
        })
    }

    pub fn create_namespace(&self, name: Atom) -> Option<Rc<Namespace>> {
        let ns = Rc::new(Namespace::default());

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

    pub fn unwrap_values(self) -> RefCell<HashMap<Atom, ScopeValueData>> {
        self.values
    }
}

#[derive(Debug, Clone)]
pub struct ScopeValueData {
    pub ty: Type,
    pub is_mut: bool,
}
