use crate::ast::{expr::Expression, literal::Literal, program::Program, stmt::Statement};

#[derive(Debug, Default)]
pub struct Codegen {}

impl Codegen {
    pub fn run(&self, program: Program) -> String {
        let mut output = String::new();

        for stmt in program.stmts {
            output.push_str(&self.statement(stmt));
        }

        output
    }

    fn statement(&self, stmt: Statement) -> String {
        match stmt {
            Statement::Return(expr) => {
                let expr = self.expression(&expr);
                format!("return {expr};")
            }
            Statement::Expression(expr) => {
                let expr = self.expression(&expr);
                format!("{expr};")
            }
            Statement::VariableDeclaration {
                is_const,
                ident,
                expression,
            } => {
                let expr = self.expression(&expression);
                format!("{} {ident}={expr};", if is_const { "const" } else { "let" })
            }
            Statement::FunctionDeclaration {
                is_async,
                ident,
                params,
                body,
            } => {
                let body = body
                    .into_iter()
                    .map(|stmt| self.statement(stmt))
                    .fold(String::new(), |acc, expr| acc + &expr);
                let params = if params.is_empty() {
                    "".to_string()
                } else {
                    params[..params.len() - 1]
                        .into_iter()
                        .fold(String::new(), |acc, expr| format!("{acc}{expr},"))
                        + &params[params.len() - 1]
                };

                format!(
                    "{}function {ident}({params}){{{body}}}",
                    if is_async { "async " } else { "" }
                )
            }
            Statement::Assignment { target, expr } => {
                let target = self.expression(&target);
                let expr = self.expression(&expr);

                format!("{target}={expr};")
            }
            Statement::Block(stmts) => {
                let stmts = stmts
                    .into_iter()
                    .map(|stmt| self.statement(stmt))
                    .collect::<String>();
                format!("{{{stmts}}}")
            }
        }
    }

    fn expression(&self, expr: &Expression) -> String {
        match expr {
            Expression::Identifier(ident) => ident.to_string(),
            Expression::Literal(literal) => match literal {
                Literal::Bool(value) => value.to_string(),
                Literal::Float(value) => value.to_string(),
                Literal::Integer(value) => value.to_string(),
                Literal::String(value) => format!("{value}"),
                Literal::Void => "".to_string(),
            },
            Expression::Grouping(expr) => {
                let expr = self.expression(expr);
                format!("({expr})")
            }
            Expression::FunctionCall { expr, args } => {
                let expr = self.expression(expr);
                let args = if args.is_empty() {
                    "".to_string()
                } else {
                    args[..args.len() - 1]
                        .into_iter()
                        .map(|arg| self.expression(arg))
                        .fold(String::new(), |acc, expr| format!("{acc}{expr},"))
                        + &self.expression(&args[args.len() - 1])
                };
                format!("{expr}({args})")
            }
            Expression::MemberAccess { expr, ident } => {
                let expr = self.expression(expr);
                format!("{expr}.{ident}")
            }
            Expression::MethodAccess { expr, ident } => {
                let expr = self.expression(expr);
                format!("{expr}.prototype.{ident}")
            }
            Expression::Unary { operator, expr } => {
                let expr = self.expression(expr);
                format!("{operator}{expr}")
            }
            Expression::Binary {
                operator,
                left,
                right,
            } => {
                let left = self.expression(left);
                let right = self.expression(right);
                format!("{left}{operator}{right}")
            }
            Expression::New { expr } => {
                let expr = self.expression(expr);
                format!("new {expr}")
            }
        }
    }
}
