use common::{
    effects::Effect,
    literal::LiteralValue,
    ops::{BinaryOperator, UnaryOperator},
};

use crate::{stmt::TypedStatement, types::Type};

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
    MemberAccess {
        expr: Box<TypedExpression>,
        ident: String,
    },
    StructInit {
        use_default: bool,
        ty: Type,
        fields: Vec<(String, TypedExpression)>,
    },
    MethodAccess {
        ty: Type,
        ident: String,
    },
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: LiteralValue,
    pub ty: Type,
}
