use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Default, Clone)]
pub struct Namegen {
    counter: Rc<RefCell<u64>>,
}

impl Namegen {
    pub fn get(&self) -> String {
        let mut counter = self.counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        format!("v{id}")
    }
}
