use ast::types::Type as AstType;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use tcast::types::Type as TcAstType;

use tcast::types::Type;

#[derive(Debug, Default)]
pub struct TypeResolver {
    types: Rc<RefCell<HashMap<String, Type>>>,
}

impl TypeResolver {
    pub fn resolve(&self, ty: &AstType) -> Option<TcAstType> {
        if let AstType::Custom(ty) = ty {
            return self
                .types
                .borrow()
                .get(ty)
                .cloned()
                .map(|of| Type::StructInstance { of: Box::new(of) });
        }

        Some(match ty {
            AstType::Bool => TcAstType::Bool,
            AstType::Char => TcAstType::Char,
            AstType::Int8 => TcAstType::Int8,
            AstType::Int16 => TcAstType::Int16,
            AstType::Int32 => TcAstType::Int32,
            AstType::Int64 => TcAstType::Int64,
            AstType::IntN => TcAstType::Int32,
            AstType::UInt8 => TcAstType::UInt8,
            AstType::UInt16 => TcAstType::UInt16,
            AstType::UInt32 => TcAstType::UInt32,
            AstType::UInt64 => TcAstType::UInt64,
            AstType::String => TcAstType::String,
            AstType::Float32 => TcAstType::Float32,
            AstType::Float64 => TcAstType::Float64,
            AstType::FloatN => TcAstType::Float32,
            AstType::Unit => TcAstType::Unit,
            _ => unreachable!(),
        })
    }

    pub fn resolve_alias(&self, alias: &str) -> Option<TcAstType> {
        return self.types.borrow().get(alias).cloned();
    }

    pub fn insert(&self, alias: String, ty: TcAstType) -> bool {
        self.types.borrow_mut().insert(alias, ty).is_some()
    }
}
