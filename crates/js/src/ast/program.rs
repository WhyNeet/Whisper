use super::stmt::Statement;

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Statement>,
}
