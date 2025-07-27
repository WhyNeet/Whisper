pub mod resolver;
pub mod scope;

use std::{cell::RefCell, collections::HashSet, iter, rc::Rc};

use ast::{
    expr::Expression as AstExpression,
    module::Module as AstModule,
    stmt::{Statement as AstStatement, StructField, StructMethod},
    types::Type as AstType,
};
use common::{annotations::Annotation, effects::Effect};
use tcast::{
    expr::{Expression, Literal, TypedExpression},
    module::Module,
    ops::{TypedBinaryOperator, TypedUnaryOperator},
    stmt::{Statement, StructField as TStructField, StructMethod as TStructMethod, TypedStatement},
    types::Type as TcAstType,
};

use crate::scope::Scope;

#[derive(Default)]
pub struct Checker {
    scope: RefCell<Rc<Scope>>,
    main_fn: RefCell<Option<String>>,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            scope: RefCell::new(Rc::new(Scope::root())),
            ..Default::default()
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
                let expr = self.expression(expr, None);

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
                is_extern,
            } => {
                if annotations.contains(&Annotation::Main) {
                    if let Some(main_fn) = &*self.main_fn.borrow() {
                        panic!("Main function is already defined (`{main_fn}`)");
                    }
                    self.main_fn.replace(Some(name.to_string()));
                }

                if *is_extern && body.is_some() {
                    panic!("Extern function cannot have a body.");
                }

                if !*is_extern && body.is_none() {
                    panic!("Function must have a body.");
                }

                self.fn_declaration(
                    name,
                    parameters,
                    body.as_ref(),
                    return_type,
                    effects,
                    *is_extern,
                )
            }
            AstStatement::VariableDeclaration {
                name,
                is_mut,
                expr,
                ty,
            } => self.var_declaration(name, expr, *is_mut, ty),
            AstStatement::StructDeclaration { name, fields } => {
                self.struct_declaration(name, fields)
            }
            AstStatement::Impl { ident, methods } => self.impl_stmt(ident, methods),
            AstStatement::Namespace { name, stmts } => self.namespace_stmt(name.clone(), stmts),
        }
    }

    fn namespace_stmt(&self, name: String, stmts: &[AstStatement]) -> TypedStatement {
        let scope = self.scope.borrow();
        let namespace = scope
            .create_namespace(name, Rc::clone(&*scope))
            .expect("Namespace is already declared");
        drop(scope);
        let prev_scope = self.scope.replace(namespace);

        let stmts = stmts.iter().map(|stmt| self.statement(stmt)).collect();

        self.scope.replace(prev_scope);

        TypedStatement {
            effects: vec![],
            stmt: Statement::Namespace { stmts },
        }
    }

    fn impl_stmt(&self, ident: &str, methods: &[StructMethod]) -> TypedStatement {
        let methods = methods
            .iter()
            .map(|method| {
                let parameters = method
                    .parameters
                    .iter()
                    .map(|(name, ty)| {
                        (
                            name.clone(),
                            self.scope
                                .borrow()
                                .type_resolver()
                                .resolve_ast_type(ty)
                                .expect("Failed to resolve type"),
                        )
                    })
                    .collect::<Vec<_>>();

                TStructMethod {
                    name: method.name.clone(),
                    annotations: method.annotations.clone(),
                    body: {
                        let prev_scope = self
                            .scope
                            .replace_with(|scope| Scope::new(Rc::clone(scope)));
                        let scope = self.scope.borrow();
                        for (name, ty) in parameters.iter() {
                            scope.insert(name.clone(), ty.clone());
                        }
                        drop(scope);
                        let expr = self.expression(&method.body, None);

                        self.scope.replace(prev_scope);

                        expr
                    },
                    effects: method.effects.clone(),
                    is_pub: method.is_pub,
                    parameters,
                    return_type: self
                        .scope
                        .borrow()
                        .type_resolver()
                        .resolve_ast_type(&method.return_type)
                        .expect("Failed to resolve type"),
                }
            })
            .collect::<Vec<_>>();

        let stmt = Statement::Impl {
            ident: ident.to_string(),
            methods: methods.clone(),
        };

        let scope = self.scope.borrow();
        let resolver = scope.type_resolver();
        resolver.add_impl(resolver.resolve_alias(ident).unwrap(), methods);

        TypedStatement {
            effects: vec![],
            stmt,
        }
    }

    pub fn struct_declaration(&self, name: &String, fields: &[StructField]) -> TypedStatement {
        let scope = self.scope.borrow();
        let resolver = scope.type_resolver();
        let fields = fields
            .iter()
            .map(|field| TStructField {
                name: field.name.clone(),
                ty: resolver
                    .resolve_ast_type(&field.ty)
                    .expect("Failed to resolve type"),
                is_pub: field.is_pub,
            })
            .collect::<Vec<_>>();
        let ty = TcAstType::Struct {
            alias: name.clone(),
            fields: fields
                .iter()
                .map(|field| (field.name.clone(), field.ty.clone()))
                .collect(),
        };

        let stmt = Statement::StructDeclaration {
            name: name.to_string(),
            fields: fields.clone(),
        };

        if self.scope.borrow().type_resolver().insert(name.clone(), ty) {
            panic!("Struct `{name}` is already defined in current scope.");
        }

        TypedStatement {
            effects: vec![],
            stmt,
        }
    }

    pub fn var_declaration(
        &self,
        name: &String,
        expr: &AstExpression,
        is_mut: bool,
        ty: &Option<AstType>,
    ) -> TypedStatement {
        let expr = self.expression(
            expr,
            ty.as_ref()
                .map(|ty| self.scope.borrow().type_resolver().resolve_ast_type(ty))
                .flatten()
                .as_ref(),
        );

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
        parameters: &Vec<(String, AstType)>,
        body: Option<&AstExpression>,
        return_type: &AstType,
        effects: &[Effect],
        is_extern: bool,
    ) -> TypedStatement {
        let prev_scope = self
            .scope
            .replace_with(|scope| Scope::new(Rc::clone(scope)));
        let scope = self.scope.borrow();
        let resolver = prev_scope.type_resolver();
        for (name, ty) in parameters {
            scope.insert(
                name.clone(),
                resolver
                    .resolve_ast_type(ty)
                    .unwrap_or_else(|| panic!("Failed to resolve type: {ty:?}")),
            );
        }
        let return_type = resolver
            .resolve_ast_type(return_type)
            .expect("Failed to resolve type");
        drop(scope);
        let body = body.map(|body| self.expression(body, None));

        if let Some(ref body) = body {
            if body.ty != return_type {
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
        }

        self.scope.replace(prev_scope);

        let scope = self.scope.borrow();
        let resolver = scope.type_resolver();

        let params = parameters
            .iter()
            .map(|(_, ty)| {
                resolver
                    .resolve_ast_type(ty)
                    .unwrap_or_else(|| panic!("Failed to resolve type: {ty:?}"))
            })
            .collect::<Vec<_>>();

        let parameters = parameters
            .iter()
            .zip(params.iter())
            .map(|((name, _), ty)| (name.clone(), ty.clone()))
            .collect();

        let fn_type = TcAstType::Fn {
            return_type: Box::new(return_type.clone()),
            params,
            effects: effects.to_owned(),
        };

        if self.scope.borrow().insert(name.clone(), fn_type) {
            panic!("Function `{name}` is already defined.");
        }

        let stmt = Statement::FunctionDeclaration {
            name: name.clone(),
            return_type,
            parameters,
            body,
            is_extern,
            effects: effects.to_owned(),
        };

        TypedStatement {
            effects: effects.to_owned(),
            stmt,
        }
    }

    pub fn expression(
        &self,
        expr: &AstExpression,
        expect_ty: Option<&TcAstType>,
    ) -> TypedExpression {
        match expr {
            AstExpression::Block { stmts, return_expr } => {
                let prev_scope = self
                    .scope
                    .replace_with(|scope| Scope::new(Rc::clone(scope)));

                let stmts = stmts
                    .iter()
                    .map(|stmt| self.statement(stmt))
                    .collect::<Vec<_>>();
                let return_expr = return_expr
                    .as_ref()
                    .map(|expr| self.expression(expr.as_ref(), None))
                    .map(Box::new);
                let return_type = return_expr
                    .as_ref()
                    .map(|expr| expr.ty.clone())
                    .unwrap_or(TcAstType::Unit);
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
                let left = self.expression(left, None);
                let right = self.expression(right, None);

                let effects = [left.effects.as_slice(), right.effects.as_slice()].concat();

                let operator: TypedBinaryOperator = (*operator).into();

                if !operator.accepts_type(&left.ty, &right.ty) {
                    panic!(
                        "{} cannot be used on types `{:?}` and `{:?}`",
                        *operator, left.ty, right.ty
                    );
                }

                let result_ty = operator.result_type(&left.ty, &right.ty);

                let expr = Expression::Binary {
                    operator: *operator,
                    left: Box::new(left),
                    right: Box::new(right),
                };

                TypedExpression {
                    effects,
                    expr,
                    ty: result_ty,
                }
            }
            AstExpression::Unary { operator, expr } => {
                let expr = self.expression(expr, None);

                let effects = expr.effects.clone();

                let operator: TypedUnaryOperator = (*operator).into();

                if !operator.accepts_type(&expr.ty) {
                    panic!("{} cannot be used on type `{:?}`", *operator, expr.ty);
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
                let expr = self.expression(expr, expect_ty);
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
                let ty = expect_ty
                    .map(|ty| {
                        if ty.is_whole() && literal.ty.is_whole() {
                            Some(ty.clone())
                        } else {
                            None
                        }
                    })
                    .flatten()
                    .or_else(|| {
                        self.scope
                            .borrow()
                            .type_resolver()
                            .resolve_ast_type(&literal.ty)
                    })
                    .unwrap_or_else(|| panic!("Failed to resolve type: {:?}", literal.ty));

                TypedExpression {
                    expr: Expression::Literal(Literal {
                        ty: ty.clone(),
                        value: literal.value.clone(),
                    }),
                    ty,
                    effects: vec![],
                }
            }
            AstExpression::Identifier(ident) => {
                let scope = self.scope.borrow();
                let resolver = scope.type_resolver();
                let Some(ty) = scope.get(ident).or_else(|| {
                    resolver
                        .resolve_alias(ident)
                        .and_then(|ty| resolver.resolve_impl(ty))
                        .map(|impls| {
                            impls
                                .into_iter()
                                .flat_map(|i| i.methods)
                                .map(|method| {
                                    (
                                        method.name,
                                        TcAstType::Fn {
                                            return_type: Box::new(method.return_type),
                                            params: method
                                                .parameters
                                                .into_iter()
                                                .map(|(_, ty)| ty)
                                                .collect(),
                                            effects: method.effects,
                                        },
                                    )
                                })
                                .collect()
                        })
                        .map(|methods| TcAstType::Struct {
                            alias: ident.clone(),
                            fields: methods,
                        })
                }) else {
                    panic!("`{ident}` is not present in current scope.")
                };

                TypedExpression {
                    expr: Expression::Identifier(ident.clone()),
                    ty,
                    effects: vec![],
                }
            }
            AstExpression::FunctionCall { expr, args } => {
                let expr = self.expression(expr, None);

                let Some((fn_return_type, fn_params, fn_effects)) = expr.ty.clone().as_fn() else {
                    panic!("`{expr:?}` is not callable.")
                };

                let args = args
                    .iter()
                    .zip(fn_params.iter())
                    .map(|(arg, ty)| self.expression(arg, Some(ty)))
                    .collect::<Vec<_>>();
                if args.len() != fn_params.len() {
                    panic!(
                        "Invalid number of arguments: expected {}, found {}",
                        args.len(),
                        fn_params.len()
                    );
                }

                for i in 0..args.len() {
                    if args[i].ty != fn_params[i] && !fn_params[i].can_infer(&args[i].ty) {
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
                let expr = self.expression(expr, None);
                let Some((alias, fields)) = expr
                    .ty
                    .clone()
                    .as_struct()
                    .or_else(|| expr.ty.clone().as_namespace())
                else {
                    panic!("Expression does not contain member `{ident}`");
                };

                let scope = self.scope.borrow();
                let resolver = scope.type_resolver();
                let impls = resolver
                    .resolve_alias(&alias)
                    .map(|ty| resolver.resolve_impl(ty));

                let Some(field_ty) = fields
                    .into_iter()
                    .find(|(name, _)| name == ident)
                    .map(|(_, ty)| ty)
                    .or_else(|| {
                        if let Some(impls) = impls {
                            impls
                                .and_then(|impls| {
                                    impls.into_iter().find_map(|i| {
                                        i.methods.into_iter().find(|i| i.name == *ident).map(
                                            |method| {
                                                (
                                                    method.name,
                                                    TcAstType::Fn {
                                                        return_type: Box::new(method.return_type),
                                                        params: method
                                                            .parameters
                                                            .into_iter()
                                                            .map(|(_, ty)| ty)
                                                            .collect(),
                                                        effects: method.effects,
                                                    },
                                                )
                                            },
                                        )
                                    })
                                })
                                .map(|(_, ty)| ty)
                        } else {
                            scope.get(ident)
                        }
                    })
                else {
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
            AstExpression::MethodAccess { expr, ident } => {
                let expr = self.expression(expr, None);

                let (_, ty) = expr
                    .ty
                    .clone()
                    .as_struct()
                    .expect("Not a struct")
                    .1
                    .into_iter()
                    .find(|(name, _)| name == ident)
                    .expect("Field is not present in struct");

                TypedExpression {
                    ty,
                    effects: vec![],
                    expr: Expression::MethodAccess {
                        ty: expr.ty,
                        ident: ident.clone(),
                    },
                }
            }
            AstExpression::StructInit {
                use_default,
                ty,
                fields,
            } => {
                let Some(ty) = self.scope.borrow().type_resolver().resolve_ast_type(ty) else {
                    panic!("Struct `{ty:?}` is not present in current scope.");
                };
                let Some((_, struct_fields)) = ty.clone().as_struct() else {
                    panic!("`{ty:?}` is not a struct.");
                };

                if *use_default {
                    todo!("use_default");
                }

                let mut fields = fields
                    .iter()
                    .map(|(name, expr)| {
                        (
                            name.clone(),
                            self.expression(
                                expr,
                                Some(
                                    struct_fields
                                        .iter()
                                        .find(|(ident, _)| ident == name)
                                        .map(|(_, ty)| ty)
                                        .unwrap(),
                                ),
                            ),
                        )
                    })
                    .collect::<Vec<_>>();

                let mut covered_fields = HashSet::new();

                for idx in 0..fields.len() {
                    let (field_name, field_expr) = fields.get_mut(idx).unwrap();

                    if covered_fields.contains(field_name) {
                        panic!("Field `{field_name}` is already defined.");
                    }

                    let field_ty = field_expr.ty.clone();
                    let Some((_, expected_ty)) =
                        struct_fields.iter().find(|(name, _)| name == field_name)
                    else {
                        panic!("Field `{field_name} does not exist on struct `{ty:?}`.");
                    };

                    if field_ty.is_inferred() && expected_ty.can_infer(&field_ty) {
                        field_expr.ty = expected_ty.clone();
                    } else if field_ty != *expected_ty {
                        panic!(
                            "Field expected expression of type `{expected_ty:?}`, but found `{field_ty:?}`."
                        );
                    }

                    covered_fields.insert(field_name.clone());
                }

                for (name, _) in struct_fields.iter() {
                    if !covered_fields.contains(name) {
                        panic!("Field `{name}` is not defined.");
                    }
                }

                let effects = fields
                    .iter()
                    .flat_map(|field| field.1.effects.as_slice())
                    .cloned()
                    .collect();

                let mut sorted_fields = vec![];

                // TODO: optimize this shit
                for (field_name, _) in struct_fields {
                    sorted_fields.push(
                        fields
                            .iter()
                            .find(|(name, _)| *name == field_name)
                            .unwrap()
                            .clone(),
                    );
                }

                let expr = Expression::StructInit {
                    use_default: *use_default,
                    ty: ty.clone(),
                    fields: sorted_fields,
                };

                TypedExpression { ty, effects, expr }
            }
        }
    }
}
