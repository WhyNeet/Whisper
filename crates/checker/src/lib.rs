pub mod scope;

use std::{cell::RefCell, iter, rc::Rc};

use ast::{
    expr::Expression as AstExpression, module::Module as AstModule, stmt::Statement as AstStatement,
};
use common::types::Type;
use tcast::{
    expr::{Expression, TypedExpression},
    module::Module,
    stmt::{Statement, TypedStatement},
};

use crate::scope::Scope;

pub struct Checker {
    scope: RefCell<Rc<Scope>>,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            scope: Default::default(),
        }
    }
}

impl Checker {
    pub fn run(&self, module: &AstModule) -> Module {
        let mut stmts = vec![];

        for stmt in module.stmts.iter() {
            stmts.push(self.statement(stmt));
        }

        Module { stmts }
    }

    pub fn statement(&self, stmt: &AstStatement) -> TypedStatement {
        match stmt {
            AstStatement::Expression { expr, .. } => {
                let expr = self.expression(expr);

                TypedStatement {
                    effects: expr.effects.clone(),
                    stmt: Statement::Expression(expr),
                }
            }
            AstStatement::FunctionDeclaration {
                name,
                return_type,
                parameters,
                body,
                effects,
            } => todo!(),
            AstStatement::VariableDeclaration { name, is_mut, expr } => todo!(),
        }
    }

    pub fn expression(&self, expr: &AstExpression) -> TypedExpression {
        match expr {
            AstExpression::Block { stmts, return_expr } => {
                let prev_scope = self
                    .scope
                    .replace_with(|scope| Rc::new(Scope::new(Rc::clone(scope))));

                let stmts = stmts
                    .into_iter()
                    .map(|stmt| self.statement(stmt))
                    .collect::<Vec<_>>();
                let return_expr = return_expr
                    .as_ref()
                    .map(|expr| self.expression(expr.as_ref()))
                    .map(Box::new);
                let return_type = return_expr
                    .as_ref()
                    .map(|expr| expr.ty)
                    .unwrap_or(Type::Unit);
                let return_expr_effects = return_expr.as_ref().map(|expr| expr.effects.as_slice());
                let effects = stmts.iter().map(|stmt| stmt.effects.as_slice());

                let effects = if let Some(return_effects) = return_expr_effects {
                    effects
                        .chain(iter::once(return_effects))
                        .flatten()
                        .cloned()
                        .collect()
                } else {
                    effects.flatten().cloned().collect()
                };

                let expr = Expression::Block { stmts, return_expr };

                self.scope.replace(prev_scope);

                TypedExpression {
                    effects,
                    expr,
                    ty: return_type,
                }
            }
            AstExpression::Binary {
                operator,
                left,
                right,
            } => {
                let left = self.expression(left);
                let right = self.expression(right);

                let effects = [left.effects.as_slice(), right.effects.as_slice()].concat();

                let left_ty = left.ty;
                let right_ty = right.ty;

                let expr = Expression::Binary {
                    operator: *operator,
                    left: Box::new(left),
                    right: Box::new(right),
                };

                if !operator.accepts_type(left_ty, right_ty) {
                    panic!(
                        "{operator} cannot be used on types `{:?}` and `{:?}`",
                        left_ty, right_ty
                    );
                }

                TypedExpression {
                    effects,
                    expr,
                    ty: operator.result_type(left_ty, right_ty),
                }
            }
            AstExpression::Unary { operator, expr } => {
                let expr = self.expression(expr);

                let effects = expr.effects.clone();

                if !operator.accepts_type(expr.ty) {
                    panic!("{operator} cannot be used on type `{:?}`", expr.ty);
                }

                let expr_ty = operator.result_type(expr.ty);

                let expr = Expression::Unary {
                    operator: *operator,
                    expr: Box::new(expr),
                };

                TypedExpression {
                    expr,
                    effects,
                    ty: expr_ty,
                }
            }
            AstExpression::Grouping(expr) => {
                let expr = self.expression(expr);
                let expr_ty = expr.ty;
                let effects = expr.effects.clone();

                let expr = Expression::Grouping(Box::new(expr));

                TypedExpression {
                    expr,
                    effects,
                    ty: expr_ty,
                }
            }
            AstExpression::Literal(literal) => {
                let ty = literal.ty;

                TypedExpression {
                    expr: Expression::Literal(literal.clone()),
                    ty,
                    effects: vec![],
                }
            }
            AstExpression::Identifier(ident) => {
                let Some(ty) = self.scope.borrow().get(ident) else {
                    panic!("Variable `{ident}` is not present in current scope.");
                };

                TypedExpression {
                    expr: Expression::Identifier(ident.clone()),
                    ty,
                    effects: vec![],
                }
            }
            AstExpression::FunctionCall { expr, args } => {
                todo!()
            }
        }
    }
}
