use crate::{expr::Expression, types::Type};
use common::effects::Effect;
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Box<ExpressionStmt>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    VariableDeclaration(Box<VariableDeclaration>),
    StructDeclaration(Box<StructDeclaration>),
    Impl(Box<Impl>),
    Namespace(Box<Namespace>),
    Import(Box<Import>),
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Atom,
    pub ty: Type,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expr: Expression,
    pub has_semi: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: Atom,
    pub return_type: Type,
    pub parameters: Vec<(Atom, Type)>,
    pub body: Option<Expression>,
    pub effects: Vec<Effect>,
    pub is_extern: bool,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: Atom,
    pub is_mut: bool,
    pub expr: Expression,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: Atom,
    pub fields: Vec<StructField>,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub ident: Atom,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub name: Atom,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<Atom>,
}
