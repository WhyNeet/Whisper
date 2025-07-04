use crate::ast::{literal::Literal, ops::UnaryOperator};

use super::ops::BinaryOperator;

pub enum Expression {
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOperation {
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
    MemberAccess {
        expr: Box<Expression>,
        ident: String,
    },
}
