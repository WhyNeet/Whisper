use crate::{expr::TypedExpression, types::Type};
use common::{annotations::Annotation, effects::Effect};
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Clone)]
pub struct TypedStatement {
    pub effects: Vec<Effect>,
    pub stmt: Statement,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(TypedExpression),
    FunctionDeclaration {
        name: Atom,
        return_type: Type,
        parameters: Vec<(Atom, Type)>,
        body: Option<TypedExpression>,
        effects: Vec<Effect>,
        is_extern: bool,
        is_pub: bool,
    },
    VariableDeclaration {
        name: Atom,
        is_mut: bool,
        expr: TypedExpression,
    },
    StructDeclaration {
        name: Atom,
        fields: Vec<StructField>,
        is_pub: bool,
    },
    Impl {
        ident: Atom,
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
    pub name: Atom,
    pub ty: Type,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct StructMethod {
    pub name: Atom,
    pub return_type: Type,
    pub parameters: Vec<(Atom, Type)>,
    pub body: TypedExpression,
    pub effects: Vec<Effect>,
    pub annotations: Vec<Annotation>,
    pub is_pub: bool,
}
