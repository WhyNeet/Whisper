use common::{annotations::Annotation, effects::Effect};

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
        body: Option<TypedExpression>,
        effects: Vec<Effect>,
        is_extern: bool,
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
    Impl {
        ident: String,
        methods: Vec<StructMethod>,
    },
    Annotated {
        annotations: Vec<Annotation>,
        stmt: Box<TypedStatement>,
    },
    Namespace {
        stmts: Vec<TypedStatement>,
    },
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct StructMethod {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<(String, Type)>,
    pub body: TypedExpression,
    pub effects: Vec<Effect>,
    pub annotations: Vec<Annotation>,
    pub is_pub: bool,
}
