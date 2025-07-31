use std::{
    cell::RefCell,
    collections::HashMap,
    sync::{Arc, atomic::AtomicU64},
};

use string_cache::DefaultAtom as Atom;

use crate::types::Type;

#[derive(Debug)]
pub struct ExternalModule {
    pub name: Atom,
    pub symbols: SymbolTable,
    pub module_path: Vec<Atom>,
}

#[derive(Debug, Default, Clone)]
pub struct SymbolTable {
    symbols: RefCell<HashMap<Atom, ModuleSymbol>>,
}

impl SymbolTable {
    pub fn insert(&self, name: Atom, symbol: ModuleSymbol) {
        self.symbols.borrow_mut().insert(name, symbol);
    }

    pub fn pairs<F>(&self, f: F)
    where
        F: Fn((&Atom, &ModuleSymbol)),
    {
        self.symbols.borrow().iter().for_each(f);
    }
}

#[derive(Debug, Clone)]
pub struct ModuleSymbol {
    pub visibility: Visibility,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public)
    }

    pub fn is_private(&self) -> bool {
        matches!(self, Visibility::Private)
    }
}

static ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ModuleId(u64);

impl ModuleId {
    pub fn new_sequential() -> Self {
        Self(ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

#[derive(Debug, Default)]
pub struct ModuleRegistry {
    modules: RefCell<HashMap<ModuleId, Option<Arc<ExternalModule>>>>,
}

impl ModuleRegistry {
    pub fn get(&self, id: &ModuleId) -> Option<Option<Arc<ExternalModule>>> {
        self.modules.borrow().get(id).cloned()
    }

    pub fn register(&self) -> ModuleId {
        let id = ModuleId::new_sequential();
        self.modules.borrow_mut().insert(id, None);

        id
    }

    pub fn insert(&self, id: ModuleId, module: ExternalModule) -> Result<(), &'static str> {
        let mut modules = self.modules.borrow_mut();

        if modules.get(&id).is_some_and(|m| m.is_some()) {
            return Err("Module is already compiled.");
        }

        modules.insert(id, Some(Arc::new(module)));

        Ok(())
    }
}
