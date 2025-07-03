use common::{
    effects::Effect,
    ops::{BinaryOperator, UnaryOperator},
    types::Type,
};

use crate::stmt::Statement;

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
        expr: Box<Expression>,
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
    Identifier(String),
    Literal(Literal),
    Block {
        stmts: Vec<Statement>,
        return_expr: Option<Box<Expression>>,
    },
    FunctionCall {
        expr: Box<Expression>,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}
