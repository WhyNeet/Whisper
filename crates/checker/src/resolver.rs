use ast::types::Type as AstType;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use tcast::stmt::StructMethod;
use tcast::types::Type as TcAstType;

use tcast::types::Type;

#[derive(Debug, Default)]
pub struct TypeResolver {
    enclosing: Option<Rc<TypeResolver>>,
    types: Rc<RefCell<HashMap<String, Type>>>,
    impls: Rc<RefCell<HashMap<String, Vec<StructMethod>>>>,
}

impl TypeResolver {
    pub fn new(enclosing: Rc<TypeResolver>) -> Self {
        Self {
            enclosing: Some(enclosing),
            ..Default::default()
        }
    }
}

impl TypeResolver {
    pub fn resolve(&self, ty: &AstType) -> Option<TcAstType> {
        if let AstType::Custom(name) = ty {
            return self
                .types
                .borrow()
                .get(name)
                .cloned()
                .or_else(|| self.enclosing.as_ref().map(|r| r.resolve(ty)).flatten());
        }

        Some(match ty {
            AstType::Bool => TcAstType::Bool,
            AstType::Char => TcAstType::Char,
            AstType::Int8 => TcAstType::Int8,
            AstType::Int16 => TcAstType::Int16,
            AstType::Int32 => TcAstType::Int32,
            AstType::Int64 => TcAstType::Int64,
            AstType::IntN => TcAstType::Infer {
                candidate: Box::new(TcAstType::Int32),
            },
            AstType::UInt8 => TcAstType::UInt8,
            AstType::UInt16 => TcAstType::UInt16,
            AstType::UInt32 => TcAstType::UInt32,
            AstType::UInt64 => TcAstType::UInt64,
            AstType::String => TcAstType::String,
            AstType::Float32 => TcAstType::Float32,
            AstType::Float64 => TcAstType::Float64,
            AstType::FloatN => TcAstType::Infer {
                candidate: Box::new(TcAstType::Float32),
            },
            AstType::Unit => TcAstType::Unit,
            _ => unreachable!(),
        })
    }

    pub fn add_impl(&self, alias: String, mut methods: Vec<StructMethod>) {
        self.impls
            .borrow_mut()
            .entry(alias)
            .or_default()
            .append(&mut methods);
    }

    pub fn resolve_impl(&self, alias: &str) -> Option<Vec<StructMethod>> {
        self.impls.borrow().get(alias).cloned().or_else(|| {
            self.enclosing
                .as_ref()
                .map(|r| r.resolve_impl(alias))
                .flatten()
        })
    }

    pub fn resolve_alias(&self, alias: &str) -> Option<TcAstType> {
        self.types.borrow().get(alias).cloned().or_else(|| {
            self.enclosing
                .as_ref()
                .map(|r| r.resolve_alias(alias))
                .flatten()
        })
    }

    pub fn insert(&self, alias: String, ty: TcAstType) -> bool {
        self.types.borrow_mut().insert(alias, ty).is_some()
    }
}
