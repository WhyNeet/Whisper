use crate::expr::Expression;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
}
