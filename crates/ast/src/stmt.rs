use crate::expr::Expression;
use common::types::Type;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    FunctionDeclaration {
        name: String,
        return_type: Type,
        parameters: Vec<(String, Type)>,
        body: Expression,
    },
}
