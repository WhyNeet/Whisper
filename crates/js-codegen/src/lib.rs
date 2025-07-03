pub mod namegen;
use ast::{
    expr::{Expression, Literal},
    module::Module,
    stmt::Statement,
};

use crate::namegen::Namegen;

pub struct JsCodegen {
    namegen: Namegen,
}

impl JsCodegen {
    pub fn new() -> Self {
        Self {
            namegen: Namegen::default(),
        }
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
        match stmt {
            Statement::Expression(expr) => self.generate_expr(expr, None) + ";",
            Statement::FunctionDeclaration {
                name,
                parameters,
                body,
                ..
            } => {
                let params = parameters[..(parameters.len() - 1)]
                    .into_iter()
                    .map(|(name, _ty)| format!("{name}"))
                    .fold(String::new(), |acc, val| format!("{acc}{val},"))
                    + &parameters[parameters.len() - 1].0;
                let return_value = self.namegen.get();
                let body = self.generate_expr(body, Some(&return_value));
                let body = format!("{body}return {return_value};");
                format!("function {name}({params}){{{body}}}")
            }
        }
    }

    fn generate_expr(&self, expr: &Expression, store_in: Option<&str>) -> String {
        let expr = match expr {
            Expression::Identifier(ident) => ident.to_string(),
            Expression::Grouping(expr) => format!("({})", self.generate_expr(expr, None)),
            Expression::Literal(literal) => self.generate_literal(literal),
            Expression::Binary {
                operator,
                left,
                right,
            } => format!(
                "{}{}{}",
                self.generate_expr(left, None),
                operator,
                self.generate_expr(right, None)
            ),
            Expression::Unary { operator, expr } => {
                format!("{}{}", operator, self.generate_expr(expr, None))
            }
        };

        if let Some(store_in) = store_in {
            format!("let {store_in}={expr};")
        } else {
            expr
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
