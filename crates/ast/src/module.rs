use super::stmt::Statement;

#[derive(Debug)]
pub struct Module {
    pub stmts: Vec<Statement>,
}
