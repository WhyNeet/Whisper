use std::{cell::RefCell, collections::HashMap, rc::Rc};
use tcast::types::Type as TcAstType;

use tcast::types::Type;

#[derive(Debug, Default)]
pub struct TypeResolver {
    types: Rc<RefCell<HashMap<Type, String>>>,
}

impl TypeResolver {
    pub fn resolve(&self, ty: &Type) -> Option<String> {
        self.types.borrow().get(ty).cloned()
    }

    pub fn insert(&self, alias: String, ty: TcAstType) -> bool {
        self.types.borrow_mut().insert(ty, alias).is_some()
    }
}
