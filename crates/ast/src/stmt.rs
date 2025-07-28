use crate::{expr::Expression, types::Type};
use common::{annotations::Annotation, effects::Effect};
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression {
        expr: Expression,
        has_semi: bool,
    },
    FunctionDeclaration {
        name: Atom,
        return_type: Type,
        is_extern: bool,
        parameters: Vec<(Atom, Type)>,
        body: Option<Expression>,
        effects: Vec<Effect>,
        is_pub: bool,
    },
    VariableDeclaration {
        name: Atom,
        is_mut: bool,
        expr: Expression,
        ty: Option<Type>,
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
    Namespace {
        name: Atom,
        stmts: Vec<Statement>,
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
    pub body: Expression,
    pub effects: Vec<Effect>,
    pub annotations: Vec<Annotation>,
    pub is_pub: bool,
}
