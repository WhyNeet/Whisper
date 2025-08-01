use std::path::PathBuf;

use crate::expr::TypedExpression;
use common::effects::Effect;
use common::module::ModuleId;
use common::types::Type;
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Clone)]
pub struct TypedStatement {
    pub effects: Vec<Effect>,
    pub stmt: Statement,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(TypedExpression),
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
pub struct FunctionDeclaration {
    pub name: Atom,
    pub return_type: Type,
    pub parameters: Vec<(Atom, Type)>,
    pub body: Option<TypedExpression>,
    pub effects: Vec<Effect>,
    pub is_extern: bool,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: Atom,
    pub is_mut: bool,
    pub expr: TypedExpression,
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
    pub stmts: Vec<TypedStatement>,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module_id: ModuleId,
    pub path: PathBuf,
    pub alias: Atom,
}
