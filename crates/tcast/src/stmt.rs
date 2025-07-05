use common::effects::Effect;

use crate::{expr::TypedExpression, types::Type};

#[derive(Debug, Clone)]
pub struct TypedStatement {
    pub effects: Vec<Effect>,
    pub stmt: Statement,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(TypedExpression),
    FunctionDeclaration {
        name: String,
        return_type: Type,
        parameters: Vec<(String, Type)>,
        body: TypedExpression,
        effects: Vec<Effect>,
    },
    VariableDeclaration {
        name: String,
        is_mut: bool,
        expr: TypedExpression,
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
