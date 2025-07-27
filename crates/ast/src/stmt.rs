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
        is_extern: bool,
        parameters: Vec<(String, Type)>,
        body: Option<Expression>,
        effects: Vec<Effect>,
        annotations: Vec<Annotation>,
    },
    VariableDeclaration {
        name: String,
        is_mut: bool,
        expr: Expression,
        ty: Option<Type>,
    },
    StructDeclaration {
        name: String,
        fields: Vec<StructField>,
    },
    Impl {
        ident: String,
        methods: Vec<StructMethod>,
    },
    Namespace {
        name: String,
        stmts: Vec<Statement>,
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
    pub body: Expression,
    pub effects: Vec<Effect>,
    pub annotations: Vec<Annotation>,
    pub is_pub: bool,
}
