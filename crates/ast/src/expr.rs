use crate::stmt::Statement;
use common::{
    literal::Literal,
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
}
