use crate::ops::{BinaryOperator, UnaryOperator};

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
}

pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}
