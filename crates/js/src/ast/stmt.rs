use crate::ast::expr::Expression;

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
}
