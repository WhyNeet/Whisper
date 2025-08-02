use std::path::PathBuf;

use crate::ast::expr::Expression;
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionDeclaration {
        is_async: bool,
        ident: Atom,
        params: Vec<Atom>,
        body: Vec<Statement>,
    },
    VariableDeclaration {
        is_const: bool,
        ident: Atom,
        expression: Expression,
    },
    Block(Vec<Statement>),
    Expression(Expression),
    Return(Expression),
    Assignment {
        target: Expression,
        expr: Expression,
    },
    Export(Box<Statement>),
    ExportAllAs(Box<ExportAllAs>),
    Import(Box<Import>),
}

#[derive(Debug, Clone)]
pub struct Import {
    pub name: Atom,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct ExportAllAs {
    pub name: Atom,
    pub path: PathBuf,
}
