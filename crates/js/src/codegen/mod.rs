use crate::ast::{expr::Expression, literal::Literal, program::Program, stmt::Statement};

#[derive(Debug, Default)]
pub struct Codegen {}

impl Codegen {
    pub fn run(&self, program: Program) -> String {
        let mut output = String::new();

        for stmt in program.stmts {
            output.push_str(&Self::statement(stmt));
        }

        output
    }

    fn statement(stmt: Statement) -> String {
        match stmt {
            Statement::Return(expr) => {
                let expr = Self::expression(&expr);
                format!("return {expr};")
            }
            Statement::Expression(expr) => {
                let expr = Self::expression(&expr);
                format!("{expr};")
            }
            Statement::VariableDeclaration {
                is_const,
                ident,
                expression,
            } => {
                let expr = Self::expression(&expression);
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
                    .map(Self::statement)
                    .fold(String::new(), |acc, expr| acc + &expr);
                let params = if params.is_empty() {
                    "".to_string()
                } else {
                    params[..params.len() - 1]
                        .iter()
                        .fold(String::new(), |acc, expr| format!("{acc}{expr},"))
                        + &params[params.len() - 1]
                };

                format!(
                    "{}function {ident}({params}){{{body}}}",
                    if is_async { "async " } else { "" }
                )
            }
            Statement::Assignment { target, expr } => {
                let target = Self::expression(&target);
                let expr = Self::expression(&expr);

                format!("{target}={expr};")
            }
            Statement::Block(stmts) => {
                let stmts = stmts.into_iter().map(Self::statement).collect::<String>();
                format!("{{{stmts}}}")
            }
            Statement::Export(stmt) => {
                let stmt = Self::statement(*stmt);
                format!("export {stmt};")
            }
            Statement::Import(import) => {
                format!("import*as {} from\"{}\";", import.name, import.path)
            }
        }
    }

    fn expression(expr: &Expression) -> String {
        match expr {
            Expression::Identifier(ident) => ident.to_string(),
            Expression::Literal(literal) => match literal {
                Literal::Bool(value) => value.to_string(),
                Literal::Float(value) => value.to_string(),
                Literal::Integer(value) => value.to_string(),
                Literal::String(value) => value.to_string(),
                Literal::Void => "".to_string(),
            },
            Expression::Grouping(expr) => {
                let expr = Self::expression(expr);
                format!("({expr})")
            }
            Expression::FunctionCall { expr, args } => {
                let expr = Self::expression(expr);
                let args = if args.is_empty() {
                    "".to_string()
                } else {
                    args[..args.len() - 1]
                        .iter()
                        .map(Self::expression)
                        .fold(String::new(), |acc, expr| format!("{acc}{expr},"))
                        + &Self::expression(&args[args.len() - 1])
                };
                format!("{expr}({args})")
            }
            Expression::MemberAccess { expr, ident } => {
                let expr = Self::expression(expr);
                format!("{expr}.{ident}")
            }
            Expression::MethodAccess { expr, ident } => {
                let expr = Self::expression(expr);
                format!("{expr}_{ident}")
            }
            Expression::Unary { operator, expr } => {
                let expr = Self::expression(expr);
                format!("{operator}{expr}")
            }
            Expression::Binary {
                operator,
                left,
                right,
            } => {
                let left = Self::expression(left);
                let right = Self::expression(right);
                format!("{left}{operator}{right}")
            }
            Expression::New { expr } => {
                let expr = Self::expression(expr);
                format!("new {expr}")
            }
            Expression::Assignment { assignee, expr } => {
                let assignee = Self::expression(assignee);
                let expr = Self::expression(expr);

                format!("{assignee}={expr}")
            }
        }
    }
}
