use std::cell::RefCell;

use crate::types::Type;

#[derive(Debug, Default)]
pub struct Namespace {
    pub(crate) members: RefCell<Vec<(String, Type)>>,
}

impl Namespace {
    pub fn add_member(&self, name: String, ty: Type) {
        self.members.borrow_mut().push((name, ty));
    }

    pub fn take_members(&self) -> Vec<(String, Type)> {
        self.members.take()
    }
}
