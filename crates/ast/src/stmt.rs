use crate::expr::Expression;
use common::{annotations::Annotation, effects::Effect, types::Type};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression {
        expr: Expression,
        has_semi: bool,
    },
    FunctionDeclaration {
        name: String,
        return_type: Type,
        parameters: Vec<(String, Type)>,
        body: Expression,
        effects: Vec<Effect>,
        annotations: Vec<Annotation>,
    },
    VariableDeclaration {
        name: String,
        is_mut: bool,
        expr: Expression,
    },
}
