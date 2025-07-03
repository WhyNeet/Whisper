pub mod namegen;
use ast::{
    expr::{Expression, Literal},
    module::Module,
    stmt::Statement,
};
use common::types::Type;

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
            Statement::Expression { expr, .. } => self.generate_expr(expr, None) + ";",
            Statement::FunctionDeclaration {
                name,
                parameters,
                body,
                return_type,
            } => {
                let params = if parameters.is_empty() {
                    String::new()
                } else {
                    parameters[..(parameters.len() - 1)]
                        .into_iter()
                        .map(|(name, _ty)| format!("{name}"))
                        .fold(String::new(), |acc, val| format!("{acc}{val},"))
                        + &parameters[parameters.len() - 1].0
                };
                let body = if *return_type == Type::Unit {
                    self.generate_expr(body, None)
                } else {
                    let return_value = self.namegen.get();
                    let body = self.generate_expr(body, Some(&return_value));
                    format!("let {return_value};{body}return {return_value};")
                };
                format!("function {name}({params}){{{body}}}")
            }
            Statement::VariableDeclaration { name, expr, .. } => match expr {
                Expression::Block { .. } => {
                    let expr = self.generate_expr(expr, Some(name));
                    format!("let {name};{expr}")
                }
                _ => {
                    let expr = self.generate_expr(expr, None);
                    format!("let {name}={expr};")
                }
            },
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
            Expression::Block { stmts, return_expr } => {
                let stmts = if stmts.is_empty() {
                    String::new()
                } else {
                    stmts.into_iter().map(|stmt| self.generate_stmt(stmt)).fold(
                        String::new(),
                        |mut acc, val| {
                            acc.push_str(&val);
                            acc
                        },
                    )
                };
                let return_expr = if let Some(return_expr) = return_expr {
                    if let Some(store_in) = store_in {
                        let expr = self.generate_expr(return_expr, Some(store_in));

                        expr
                    } else {
                        String::new()
                    }
                } else {
                    String::new()
                };

                format!("{{{stmts}{return_expr}}}")
            }
            Expression::FunctionCall { expr, args } => {
                let (expr, store_in) = match expr.as_ref() {
                    Expression::Identifier(ident) => (ident.to_string(), None),
                    _ => {
                        let store_in = self.namegen.get();
                        let expr = self.generate_expr(expr, Some(&store_in));
                        (expr, Some(store_in))
                    }
                };
                let args = if args.is_empty() {
                    String::new()
                } else {
                    args[..(args.len() - 1)]
                        .into_iter()
                        .map(|expr| self.generate_expr(expr, None))
                        .fold(String::new(), |acc, val| format!("{acc}{val},"))
                        + &self.generate_expr(&args[args.len() - 1], None)
                };

                format!(
                    "{}({args})",
                    if let Some(store_in) = store_in {
                        store_in
                    } else {
                        expr
                    }
                )
            }
        };

        if !expr.starts_with("{") {
            if let Some(store_in) = store_in {
                format!("{store_in}={expr};")
            } else {
                expr
            }
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
