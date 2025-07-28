use super::ops::BinaryOperator;
use crate::ast::{literal::Literal, ops::UnaryOperator};
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        expr: Box<Expression>,
    },
    Identifier(Atom),
    Grouping(Box<Expression>),
    Literal(Literal),
    FunctionCall {
        expr: Box<Expression>,
        args: Vec<Expression>,
    },
    New {
        expr: Box<Expression>,
    },
    MemberAccess {
        expr: Box<Expression>,
        ident: Atom,
    },
    MethodAccess {
        expr: Box<Expression>,
        ident: Atom,
    },
    Assignment {
        assignee: Box<Expression>,
        expr: Box<Expression>,
    },
}
