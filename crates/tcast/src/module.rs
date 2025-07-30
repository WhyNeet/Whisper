use crate::stmt::TypedStatement;
use common::module::SymbolTable;
use string_cache::DefaultAtom as Atom;

#[derive(Debug)]
pub struct Module {
    pub stmts: Vec<TypedStatement>,
    pub entrypoint: Option<Atom>,
    pub symbols: SymbolTable,
}
