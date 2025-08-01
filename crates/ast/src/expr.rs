use crate::{stmt::Statement, types::Type};
use common::{
    literal::LiteralValue,
    ops::{BinaryOperator, UnaryOperator},
};
use string_cache::DefaultAtom as Atom;

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
    Identifier(Atom),
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
        ident: Atom,
    },
    StructInit {
        use_default: bool,
        ty: Type,
        fields: Vec<(Atom, Expression)>,
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

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: LiteralValue,
    pub ty: Type,
}
