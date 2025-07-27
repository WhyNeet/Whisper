use std::{cell::RefCell, collections::HashMap, rc::Rc};
use tcast::stmt::StructMethod;
use tcast::types::Type;

#[derive(Debug, Default)]
pub struct TypeResolver {
    enclosing: Option<Rc<TypeResolver>>,
    alias_mapping: RefCell<HashMap<String, Type>>,
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
            ("UInt", Type::UInt),
            ("Int", Type::Int),
            ("Float", Type::Float),
            ("Bool", Type::Bool),
            ("Char", Type::Char),
            ("String", Type::String),
            ("Unit", Type::Unit),
        ];
        for (name, ty) in pairs {
            alias_mapping.insert(name.to_string(), ty);
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

    pub fn add_impl(&self, ty: Type, methods: Vec<StructMethod>) {
        self.impls
            .borrow_mut()
            .entry(ty)
            .or_default()
            .push(StructImpl { methods });
    }

    pub fn resolve_impl(&self, ty: Type) -> Option<Vec<StructImpl>> {
        self.impls
            .borrow()
            .get(&ty)
            .cloned()
            .or_else(|| self.enclosing.as_ref().and_then(|r| r.resolve_impl(ty)))
    }

    pub fn resolve_alias(&self, alias: &str) -> Option<Type> {
        self.alias_mapping
            .borrow()
            .get(alias)
            .cloned()
            .or_else(|| self.enclosing.as_ref().and_then(|r| r.resolve_alias(alias)))
    }

    pub fn insert(&self, alias: String, ty: Type) -> bool {
        self.alias_mapping.borrow_mut().insert(alias, ty).is_some()
    }
}

#[derive(Debug, Clone)]
pub struct StructImpl {
    pub(crate) methods: Vec<StructMethod>,
    // May add trait name here later
}
