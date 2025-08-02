use common::types::Type;
use std::cell::RefCell;
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Default, Clone)]
pub struct Namespace {
    pub(crate) members: RefCell<Vec<(Atom, Type)>>,
}

impl Namespace {
    pub fn add_member(&self, name: Atom, ty: Type) {
        self.members.borrow_mut().push((name, ty));
    }

    pub fn take_members(&self) -> Vec<(Atom, Type)> {
        self.members.take()
    }
}
