use ast::{
    expr::{Expression, Literal},
    module::Module,
    stmt::Statement,
};

pub struct JsCodegen {}

impl JsCodegen {
    pub fn new() -> Self {
        Self {}
    }
}

impl JsCodegen {
    pub fn generate_module(&self, module: &Module) -> String {
        let mut output = String::new();

        for stmt in module.stmts.iter() {
            output.push_str(&self.generate_stmt(stmt));
        }

        output
    }

    fn generate_stmt(&self, stmt: &Statement) -> String {
        (match stmt {
          Statement::Expression(expr) => self.generate_expr(expr)
        }) + ";"
    }

    fn generate_expr(&self, expr: &Expression) -> String {
        match expr {
            Expression::Identifier(ident) => ident.to_string(),
            Expression::Grouping(expr) => format!("({})", self.generate_expr(expr)),
            Expression::Literal(literal) => self.generate_literal(literal),
            Expression::Binary {
                operator,
                left,
                right,
            } => format!("{}{}{}", self.generate_expr(left), operator, self.generate_expr(right)),
            Expression::Unary { operator, expr } => format!("{}{}", operator, self.generate_expr(expr))
        }
    }

    fn generate_literal(&self, literal: &Literal) -> String {
        match literal {
            Literal::Bool(value) => value.to_string(),
            Literal::Float(value) => value.to_string(),
            Literal::Integer(value) => value.to_string(),
            Literal::String(value) => value.to_string(),
        }
    }
}
