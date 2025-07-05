use crate::{expr::Expression, types::Type};
use common::{annotations::Annotation, effects::Effect};

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
    StructDeclaration {
        name: String,
        fields: Vec<StructField>,
    },
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub is_pub: bool,
}
