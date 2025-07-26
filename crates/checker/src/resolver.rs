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
            ("u8", Type::UInt8),
            ("u16", Type::UInt16),
            ("u32", Type::UInt32),
            ("u64", Type::UInt64),
            ("i8", Type::Int8),
            ("i16", Type::Int16),
            ("i32", Type::Int32),
            ("i64", Type::Int64),
            ("f32", Type::Float32),
            ("f64", Type::Float64),
            ("bool", Type::Bool),
            ("char", Type::Char),
            ("string", Type::String),
            ("unit", Type::Unit),
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
            _ => None,
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
        self.impls.borrow().get(&ty).cloned().or_else(|| {
            self.enclosing
                .as_ref()
                .map(|r| r.resolve_impl(ty))
                .flatten()
        })
    }

    pub fn resolve_alias(&self, alias: &str) -> Option<Type> {
        self.alias_mapping.borrow().get(alias).cloned().or_else(|| {
            self.enclosing
                .as_ref()
                .map(|r| r.resolve_alias(alias))
                .flatten()
        })
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
