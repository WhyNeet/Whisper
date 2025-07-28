use crate::stmt::TypedStatement;

#[derive(Debug)]
pub struct Module {
    pub stmts: Vec<TypedStatement>,
    pub entrypoint: Option<String>,
}
