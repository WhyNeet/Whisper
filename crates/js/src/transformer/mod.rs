pub mod resolver;

use std::{cell::RefCell, rc::Rc};

use common::{literal::LiteralValue, ops::UnaryOperator as LangUnaryOperator};
use tcast::{
    expr::{Expression as TExpression, TypedExpression},
    module::Module,
    stmt::{Statement as TStatement, StructField, TypedStatement},
    types::Type,
};

use crate::{
    ast::{
        expr::Expression,
        literal::Literal,
        ops::{BinaryOperator, UnaryOperator},
        program::Program,
        stmt::Statement,
    },
    transformer::resolver::TypeResolver,
};

#[derive(Debug, Default)]
pub struct TypedAstTransformer {
    type_resolver: RefCell<Rc<TypeResolver>>,
}

impl TypedAstTransformer {
    pub fn run(&self, module: &Module) -> Program {
        let mut stmts = vec![];

        for stmt in module.stmts.iter() {
            stmts.append(&mut self.statement(stmt));
        }

        stmts.push(Statement::Expression(Expression::FunctionCall {
            expr: Box::new(Expression::Identifier(module.entrypoint.to_string())),
            args: vec![],
        }));

        Program { stmts }
    }

    fn statement(&self, stmt: &TypedStatement) -> Vec<Statement> {
        match &stmt.stmt {
            TStatement::Expression(expr) => {
                if stmt.effects.is_empty() {
                    return vec![];
                }

                let (mut tail, expr) = self.expression(expr);
                tail.push(Statement::Expression(expr));

                tail
            }
            TStatement::FunctionDeclaration {
                name,
                parameters,
                body,
                ..
            } => {
                if let Some(body) = body {
                    vec![self.fn_declaration(name, parameters, body)]
                } else {
                    vec![]
                }
            }
            TStatement::VariableDeclaration { name, is_mut, expr } => {
                self.var_declaration(name, expr, *is_mut)
            }
            TStatement::StructDeclaration { name, fields } => {
                self.type_resolver.borrow().insert(
                    name.clone(),
                    Type::Struct {
                        fields: fields
                            .iter()
                            .map(|field| (field.name.clone(), field.ty.clone()))
                            .collect(),
                        alias: name.clone(),
                    },
                );
                vec![self.struct_declaration(name, fields)]
            }
            TStatement::Impl { ident, methods } => methods
                .iter()
                .map(|method| {
                    self.fn_declaration(
                        &format!("{ident}_{}", method.name),
                        &method.parameters,
                        &method.body,
                    )
                })
                .chain(methods.iter().map(|method| Statement::Assignment {
                    target: Expression::MemberAccess {
                        expr: Box::new(Expression::MemberAccess {
                            expr: Box::new(Expression::Identifier(ident.clone())),
                            ident: "prototype".to_string(),
                        }),
                        ident: method.name.clone(),
                    },
                    expr: Expression::Identifier(format!("{ident}_{}", method.name)),
                }))
                .collect(),
            TStatement::Annotated { .. } => todo!(),
            TStatement::Namespace { .. } => vec![],
        }
    }

    fn struct_declaration(&self, name: &String, fields: &[StructField]) -> Statement {
        let body = fields
            .iter()
            .map(|field| Statement::Assignment {
                target: Expression::MemberAccess {
                    expr: Box::new(Expression::Identifier("this".to_string())),
                    ident: field.name.clone(),
                },
                expr: Expression::Identifier(field.name.clone()),
            })
            .collect();

        Statement::FunctionDeclaration {
            is_async: false,
            ident: name.to_string(),
            params: fields.iter().map(|field| field.name.clone()).collect(),
            body,
        }
    }

    fn var_declaration(
        &self,
        name: &String,
        expr: &TypedExpression,
        is_mut: bool,
    ) -> Vec<Statement> {
        let (mut tail, expr) = self.expression(expr);

        tail.push(Statement::VariableDeclaration {
            is_const: !is_mut,
            ident: name.to_string(),
            expression: expr,
        });

        tail
    }

    fn fn_declaration(
        &self,
        name: &String,
        parameters: &[(String, Type)],
        body: &TypedExpression,
    ) -> Statement {
        let (stmts, ret) = self.expression(body);
        let mut body = stmts;
        if ret != Expression::Literal(Literal::Void) {
            body.push(Statement::Return(ret));
        }

        Statement::FunctionDeclaration {
            is_async: false,
            ident: name.to_string(),
            params: parameters
                .iter()
                .map(|(param, _)| param.to_string())
                .collect(),
            body,
        }
    }

    fn expression(&self, expr: &TypedExpression) -> (Vec<Statement>, Expression) {
        match &expr.expr {
            TExpression::Literal(literal) => (
                vec![],
                Expression::Literal(match &literal.value {
                    LiteralValue::String(value) => Literal::String(value.clone()),
                    LiteralValue::Bool(value) => Literal::Bool(*value),
                    LiteralValue::Float(value) => Literal::Float(*value),
                    LiteralValue::Integer(value) => Literal::Integer(*value),
                    LiteralValue::Unit => Literal::Void,
                }),
            ),
            TExpression::Identifier(ident) => (vec![], Expression::Identifier(ident.to_string())),
            TExpression::Grouping(expr) => {
                let (tail, expr) = self.expression(expr);
                (tail, Expression::Grouping(Box::new(expr)))
            }
            TExpression::Unary { operator, expr } => {
                let (tail, expr) = self.expression(expr);

                (
                    tail,
                    Expression::Unary {
                        operator: match operator {
                            LangUnaryOperator::Neg => UnaryOperator::Neg,
                            LangUnaryOperator::Not => UnaryOperator::Not,
                        },
                        expr: Box::new(expr),
                    },
                )
            }
            TExpression::Binary {
                operator,
                left,
                right,
            } => {
                let (left_tail, left) = self.expression(left);
                let (right_tail, right) = self.expression(right);

                (
                    [left_tail, right_tail].concat(),
                    Expression::Binary {
                        operator: match operator {
                            common::ops::BinaryOperator::Add => BinaryOperator::Add,
                            common::ops::BinaryOperator::Sub => BinaryOperator::Sub,
                            common::ops::BinaryOperator::Mul => BinaryOperator::Mul,
                            common::ops::BinaryOperator::Div => BinaryOperator::Div,
                            common::ops::BinaryOperator::Or => BinaryOperator::Or,
                            common::ops::BinaryOperator::And => BinaryOperator::And,
                            common::ops::BinaryOperator::BitXor => BinaryOperator::BitXor,
                            common::ops::BinaryOperator::BitOr => BinaryOperator::BitOr,
                            common::ops::BinaryOperator::BitAnd => BinaryOperator::BitAnd,
                            common::ops::BinaryOperator::Shl => BinaryOperator::Shl,
                            common::ops::BinaryOperator::Shr => BinaryOperator::Shr,
                        },
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                )
            }
            TExpression::MemberAccess { expr, ident } => {
                let (tail, expr) = self.expression(expr);

                (
                    tail,
                    Expression::MemberAccess {
                        expr: Box::new(expr),
                        ident: ident.to_string(),
                    },
                )
            }
            TExpression::MethodAccess {
                ty,
                ident: method_ident,
            } => {
                let ident = match ty {
                    Type::Struct { alias, .. } => alias,
                    _ => panic!(),
                };

                (
                    vec![],
                    Expression::MethodAccess {
                        expr: Box::new(Expression::Identifier(ident.clone())),
                        ident: method_ident.clone(),
                    },
                )
            }
            TExpression::FunctionCall { expr, args } => {
                let (tail, expr) = self.expression(expr);

                let (args_tails, args) =
                    args.iter()
                        .map(|arg| self.expression(arg))
                        .collect::<(Vec<Vec<Statement>>, Vec<Expression>)>();

                (
                    [tail, args_tails.concat()].concat(),
                    Expression::FunctionCall {
                        expr: Box::new(expr),
                        args,
                    },
                )
            }
            TExpression::Block {
                return_expr, stmts, ..
            } => {
                let mut tail = stmts
                    .iter()
                    .map(|stmt| self.statement(stmt))
                    .collect::<Vec<_>>();
                let return_expr = return_expr
                    .as_ref()
                    .map(|expr| self.expression(expr.as_ref()));

                let Some((ret_tail, expr)) = return_expr else {
                    return (tail.concat(), Expression::Literal(Literal::Void));
                };

                tail.push(ret_tail);

                (tail.concat(), expr)
            }
            TExpression::StructInit { ty, fields, .. } => {
                let (tails, exprs) = fields
                    .iter()
                    .map(|(_, expr)| {
                        let (tail, expr) = self.expression(expr);
                        (tail, expr)
                    })
                    .collect::<(Vec<_>, Vec<_>)>();
                let tail = tails.concat();

                let name = self.type_resolver.borrow().resolve(ty).unwrap();

                let expr = Expression::New {
                    expr: Box::new(Expression::FunctionCall {
                        expr: Box::new(Expression::Identifier(name)),
                        args: exprs,
                    }),
                };

                (tail, expr)
            }
        }
    }
}
