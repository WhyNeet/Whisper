pub mod scope;

use std::{cell::RefCell, iter, rc::Rc};

use ast::{
    expr::Expression as AstExpression, module::Module as AstModule, stmt::Statement as AstStatement,
};
use common::{annotations::Annotation, effects::Effect, types::Type};
use tcast::{
    expr::{Expression, TypedExpression},
    module::Module,
    stmt::{Statement, TypedStatement},
};

use crate::scope::Scope;

pub struct Checker {
    scope: RefCell<Rc<Scope>>,
    main_fn: RefCell<Option<String>>,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            scope: RefCell::new(Rc::new(Scope::js_default())),
            main_fn: Default::default(),
        }
    }
}

impl Checker {
    pub fn run(&self, module: &AstModule) -> Module {
        let mut stmts = vec![];

        for stmt in module.stmts.iter() {
            stmts.push(self.statement(stmt));
        }

        Module {
            stmts,
            entrypoint: self
                .main_fn
                .borrow()
                .as_ref()
                .expect("Entrypoint is not defined (use `@main` annotation to define entrypoint).")
                .to_string(),
        }
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
                annotations,
            } => {
                if annotations.contains(&Annotation::Main) {
                    if let Some(main_fn) = &*self.main_fn.borrow() {
                        panic!("Main function is already defined (`{main_fn}`)");
                    }
                    self.main_fn.replace(Some(name.to_string()));
                }
                self.fn_declaration(name, parameters, body, return_type, effects)
            }
            AstStatement::VariableDeclaration { name, is_mut, expr } => {
                self.var_declaration(name, expr, *is_mut)
            }
        }
    }

    pub fn var_declaration(
        &self,
        name: &String,
        expr: &AstExpression,
        is_mut: bool,
    ) -> TypedStatement {
        let expr = self.expression(expr);

        self.scope
            .borrow()
            .insert(name.to_string(), expr.ty.clone());

        let expr_effects = expr.effects.clone();

        let stmt = Statement::VariableDeclaration {
            name: name.to_string(),
            is_mut,
            expr,
        };

        TypedStatement {
            effects: expr_effects,
            stmt,
        }
    }

    pub fn fn_declaration(
        &self,
        name: &String,
        parameters: &Vec<(String, Type)>,
        body: &AstExpression,
        return_type: &Type,
        effects: &Vec<Effect>,
    ) -> TypedStatement {
        let prev_scope = self
            .scope
            .replace_with(|scope| Rc::new(Scope::new(Rc::clone(scope))));
        let scope = self.scope.borrow();
        for (name, ty) in parameters {
            scope.insert(name.clone(), ty.clone());
        }
        drop(scope);
        let body = self.expression(body);

        if body.ty != *return_type {
            panic!(
                "Expected `{name}` to return type `{return_type:?}`, but got: {:?}",
                body.ty
            );
        }

        body.effects.iter().for_each(|effect| {
            if !effects.contains(effect) {
                panic!("Missing `{effect}` in function signature.");
            }
        });

        self.scope.replace(prev_scope);

        let fn_type = Type::Fn {
            return_type: Box::new(return_type.clone()),
            params: parameters.into_iter().map(|(_, ty)| ty.clone()).collect(),
            effects: effects.clone(),
        };

        if self.scope.borrow().insert(name.clone(), fn_type) {
            panic!("Function `{name}` is already defined.");
        }

        let stmt = Statement::FunctionDeclaration {
            name: name.clone(),
            return_type: return_type.clone(),
            parameters: parameters.clone(),
            body,
            effects: effects.clone(),
        };

        TypedStatement {
            effects: effects.clone(),
            stmt,
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
                    .map(|expr| expr.ty.clone())
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

                let left_ty = left.ty.clone();
                let right_ty = right.ty.clone();

                let expr = Expression::Binary {
                    operator: *operator,
                    left: Box::new(left),
                    right: Box::new(right),
                };

                if !operator.accepts_type(&left_ty, &right_ty) {
                    panic!(
                        "{operator} cannot be used on types `{:?}` and `{:?}`",
                        left_ty, right_ty
                    );
                }

                TypedExpression {
                    effects,
                    expr,
                    ty: operator.result_type(&left_ty, &right_ty),
                }
            }
            AstExpression::Unary { operator, expr } => {
                let expr = self.expression(expr);

                let effects = expr.effects.clone();

                if !operator.accepts_type(&expr.ty) {
                    panic!("{operator} cannot be used on type `{:?}`", expr.ty);
                }

                let expr_ty = operator.result_type(&expr.ty);

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
                let expr_ty = expr.ty.clone();
                let effects = expr.effects.clone();

                let expr = Expression::Grouping(Box::new(expr));

                TypedExpression {
                    expr,
                    effects,
                    ty: expr_ty,
                }
            }
            AstExpression::Literal(literal) => {
                let ty = literal.ty.clone();

                TypedExpression {
                    expr: Expression::Literal(literal.clone()),
                    ty,
                    effects: vec![],
                }
            }
            AstExpression::Identifier(ident) => {
                let Some(ty) = self.scope.borrow().get(ident) else {
                    panic!("`{ident}` is not present in current scope.");
                };

                TypedExpression {
                    expr: Expression::Identifier(ident.clone()),
                    ty,
                    effects: vec![],
                }
            }
            AstExpression::FunctionCall { expr, args } => {
                let expr = self.expression(expr);

                let Some((fn_return_type, fn_params, fn_effects)) = expr.ty.clone().as_fn() else {
                    panic!("`{expr:?}` is not callable.")
                };

                let args = args
                    .into_iter()
                    .map(|arg| self.expression(arg))
                    .collect::<Vec<_>>();
                if args.len() != fn_params.len() {
                    panic!(
                        "Invalid number of arguments: expected {}, found {}",
                        args.len(),
                        fn_params.len()
                    );
                }

                for i in 0..args.len() {
                    if args[i].ty != fn_params[i] {
                        panic!(
                            "Invalid argument type at position {i}: expected `{:?}`, found `{:?}`",
                            fn_params[i], args[i].ty
                        );
                    }
                }

                let expr = Expression::FunctionCall {
                    expr: Box::new(expr),
                    args,
                };

                TypedExpression {
                    effects: fn_effects,
                    expr,
                    ty: fn_return_type.as_ref().clone(),
                }
            }
            AstExpression::MemberAccess { expr, ident } => {
                let expr = self.expression(expr);
                let Some(fields) = expr.ty.clone().as_struct() else {
                    panic!("Expression does not contain member `{ident}`");
                };

                let Some((_, field_ty)) = fields.into_iter().find(|(name, _)| name == ident) else {
                    panic!("Expression does not contain member `{ident}`");
                };

                let expr = Expression::MemberAccess {
                    expr: Box::new(expr),
                    ident: ident.to_string(),
                };

                TypedExpression {
                    ty: field_ty,
                    effects: vec![],
                    expr,
                }
            }
        }
    }
}
