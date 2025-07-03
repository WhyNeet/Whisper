use common::{
    effects::Effect,
    literal::Literal,
    ops::{BinaryOperator, UnaryOperator},
    types::Type,
};

use crate::stmt::TypedStatement;

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub effects: Vec<Effect>,
    pub ty: Type,
    pub expr: Expression,
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
    Identifier(String),
    Literal(Literal),
    Block {
        stmts: Vec<TypedStatement>,
        return_expr: Option<Box<TypedExpression>>,
    },
    FunctionCall {
        expr: Box<TypedExpression>,
        args: Vec<TypedExpression>,
    },
}
