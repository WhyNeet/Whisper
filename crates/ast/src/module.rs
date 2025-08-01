use super::stmt::Statement;
use common::module::ModuleId;
use string_cache::DefaultAtom as Atom;

#[derive(Debug)]
pub struct Module {
    pub stmts: Vec<Statement>,
    pub dependencies: Vec<Dependency>,
}

#[derive(Debug)]
pub struct Dependency {
    pub module_id: ModuleId,
    pub relative_path: Vec<Atom>,
}
