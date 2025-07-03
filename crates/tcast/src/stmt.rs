use common::{effects::Effect, types::Type};

use crate::expr::Expression;

pub struct TypedStatement {
    pub effects: Vec<Effect>,
    pub stmt: Statement,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    FunctionDeclaration {
        name: String,
        return_type: Type,
        parameters: Vec<(String, Type)>,
        body: Expression,
        effects: Vec<Effect>,
    },
    VariableDeclaration {
        name: String,
        is_mut: bool,
        expr: Expression,
    },
}
