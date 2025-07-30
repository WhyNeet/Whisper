use common::types::Type;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use string_cache::DefaultAtom as Atom;
use tcast::stmt::FunctionDeclaration;

#[derive(Debug, Default)]
pub struct TypeResolver {
    enclosing: Option<Rc<TypeResolver>>,
    alias_mapping: RefCell<HashMap<Atom, Type>>,
    impls: RefCell<HashMap<Type, Vec<StructImpl>>>,
}

impl TypeResolver {
    pub fn new(enclosing: Rc<TypeResolver>) -> Self {
        Self {
            enclosing: Some(enclosing),
            alias_mapping: Default::default(),
            impls: Default::default(),
        }
    }

    pub fn root() -> Self {
        let mut alias_mapping = HashMap::new();

        let pairs = [
            (Atom::from("UInt"), Type::UInt),
            (Atom::from("Int"), Type::Int),
            (Atom::from("Float"), Type::Float),
            (Atom::from("Bool"), Type::Bool),
            (Atom::from("Char"), Type::Char),
            (Atom::from("String"), Type::String),
            (Atom::from("Unit"), Type::Unit),
        ];
        for (name, ty) in pairs {
            alias_mapping.insert(name, ty);
        }

        Self {
            alias_mapping: RefCell::new(alias_mapping),
            ..Default::default()
        }
    }
}

impl TypeResolver {
    pub fn resolve_ast_type(&self, ty: &ast::types::Type) -> Option<Type> {
        match ty {
            ast::types::Type::Alias(alias) => self.resolve_alias(alias),
        }
    }

    pub fn add_impl(&self, ty: Type, methods: Vec<FunctionDeclaration>) {
        self.impls
            .borrow_mut()
            .entry(ty)
            .or_default()
            .push(StructImpl { methods });
    }

    pub fn resolve_impl(&self, ty: &Type) -> Option<Vec<StructImpl>> {
        self.impls
            .borrow()
            .get(ty)
            .cloned()
            .or_else(|| self.enclosing.as_ref().and_then(|r| r.resolve_impl(ty)))
    }

    pub fn resolve_alias(&self, alias: &Atom) -> Option<Type> {
        self.alias_mapping
            .borrow()
            .get(alias)
            .cloned()
            .or_else(|| self.enclosing.as_ref().and_then(|r| r.resolve_alias(alias)))
    }

    pub fn insert(&self, alias: Atom, ty: Type) -> bool {
        self.alias_mapping.borrow_mut().insert(alias, ty).is_some()
    }
}

#[derive(Debug, Clone)]
pub struct StructImpl {
    pub(crate) methods: Vec<FunctionDeclaration>,
    // May add trait name here later
}
