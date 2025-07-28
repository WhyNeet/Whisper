use crate::stmt::TypedStatement;
use string_cache::DefaultAtom as Atom;

#[derive(Debug)]
pub struct Module {
    pub stmts: Vec<TypedStatement>,
    pub entrypoint: Option<Atom>,
}
