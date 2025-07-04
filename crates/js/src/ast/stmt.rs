use crate::ast::expr::Expression;

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionDeclaration {
        is_async: bool,
        ident: String,
        params: Vec<String>,
        body: Vec<Statement>,
    },
    VariableDeclaration {
        is_const: bool,
        ident: String,
        expression: Expression,
    },
    Block(Vec<Statement>),
    Expression(Expression),
    Return(Expression),
}
