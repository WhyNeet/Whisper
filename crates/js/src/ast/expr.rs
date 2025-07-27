use crate::ast::{literal::Literal, ops::UnaryOperator};

use super::ops::BinaryOperator;

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
    Identifier(String),
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
        ident: String,
    },
    MethodAccess {
        expr: Box<Expression>,
        ident: String,
    },
    Assignment {
        assignee: Box<Expression>,
        expr: Box<Expression>,
    },
}
