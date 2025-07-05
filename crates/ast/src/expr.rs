use crate::{stmt::Statement, types::Type};
use common::{
    literal::LiteralValue,
    ops::{BinaryOperator, UnaryOperator},
};

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
    MemberAccess {
        expr: Box<Expression>,
        ident: String,
    },
    StructInit {
        use_default: bool,
        ty: Type,
        fields: Vec<(String, Expression)>,
    },
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: LiteralValue,
    pub ty: Type,
}
