use std::rc::Rc;
use string_cache::DefaultAtom as Atom;

use common::{
    effects::Effect,
    literal::LiteralValue,
    ops::{BinaryOperator, UnaryOperator},
};

use crate::{namespace::Namespace, stmt::TypedStatement, types::Type};

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub effects: Vec<Effect>,
    pub ty: Type,
    pub expr: Expression,
    pub is_mut: bool,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Unary {
        operator: UnaryOperator,
        expr: Box<TypedExpression>,
    },
    Binary {
        operator: BinaryOperator,
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
    },
    Grouping(Box<TypedExpression>),
    Identifier(Atom),
    Literal(Literal),
    Block {
        stmts: Vec<TypedStatement>,
        return_expr: Option<Box<TypedExpression>>,
    },
    FunctionCall {
        expr: Box<TypedExpression>,
        args: Vec<TypedExpression>,
    },
    MemberAccess {
        expr: Box<TypedExpression>,
        ident: Atom,
    },
    StructInit {
        use_default: bool,
        ty: Type,
        fields: Vec<(Atom, TypedExpression)>,
    },
    MethodAccess {
        expr: Box<TypedExpression>,
        ident: Atom,
    },
    Assignment {
        assignee: Box<TypedExpression>,
        expr: Box<TypedExpression>,
    },
    Namespace {
        alias: Atom,
        namespace: Rc<Namespace>,
    },
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: LiteralValue,
    pub ty: Type,
}
