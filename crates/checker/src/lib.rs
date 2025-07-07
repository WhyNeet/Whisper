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

use crate::{resolver::TypeResolver, scope::Scope};

pub struct Checker {
    scope: RefCell<Rc<Scope>>,
    type_resolver: RefCell<Rc<TypeResolver>>,
    main_fn: RefCell<Option<String>>,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            scope: RefCell::new(Rc::new(Scope::js_default())),
            type_resolver: Default::default(),
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
            AstStatement::StructDeclaration { name, fields } => {
                self.struct_declaration(name, fields)
            }
            AstStatement::Impl { ident, methods } => self.impl_stmt(ident, methods),
        }
    }

    fn impl_stmt(&self, ident: &String, methods: &Vec<StructMethod>) -> TypedStatement {
        let methods = methods
            .into_iter()
            .map(|method| {
                let parameters = method
                    .parameters
                    .iter()
                    .map(|(name, ty)| {
                        (
                            name.clone(),
                            self.type_resolver
                                .borrow()
                                .resolve(ty)
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
                            .replace_with(|scope| Rc::new(Scope::new(Rc::clone(scope))));
                        let prev_resolver = self
                            .type_resolver
                            .replace_with(|r| Rc::new(TypeResolver::new(Rc::clone(r))));
                        let scope = self.scope.borrow();
                        for (name, ty) in parameters.iter() {
                            scope.insert(name.clone(), ty.clone());
                        }
                        drop(scope);
                        let expr = self.expression(&method.body);

                        self.type_resolver.replace(prev_resolver);
                        self.scope.replace(prev_scope);

                        expr
                    },
                    effects: method.effects.clone(),
                    is_pub: method.is_pub,
                    parameters,
                    return_type: self
                        .type_resolver
                        .borrow()
                        .resolve(&method.return_type)
                        .expect("Failed to resolve type"),
                }
            })
            .collect::<Vec<_>>();

        let stmt = Statement::Impl {
            ident: ident.clone(),
            methods: methods.clone(),
        };

        self.type_resolver.borrow().add_impl(ident.clone(), methods);

        TypedStatement {
            effects: vec![],
            stmt,
        }
    }

    pub fn struct_declaration(&self, name: &String, fields: &Vec<StructField>) -> TypedStatement {
        let fields = fields
            .into_iter()
            .map(|field| TStructField {
                name: field.name.clone(),
                ty: self
                    .type_resolver
                    .borrow()
                    .resolve(&field.ty)
                    .expect("Cannot resolve type"),
                is_pub: field.is_pub,
            })
            .collect::<Vec<_>>();
        let ty = TcAstType::Struct {
            fields: fields
                .iter()
                .map(|field| (field.name.clone(), field.ty.clone()))
                .collect(),
        };

        let stmt = Statement::StructDeclaration {
            name: name.to_string(),
            fields: fields.clone(),
        };

        if self.type_resolver.borrow().insert(name.clone(), ty) {
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
        parameters: &Vec<(String, AstType)>,
        body: &AstExpression,
        return_type: &AstType,
        effects: &Vec<Effect>,
    ) -> TypedStatement {
        let prev_scope = self
            .scope
            .replace_with(|scope| Rc::new(Scope::new(Rc::clone(scope))));
        let prev_resolver = self
            .type_resolver
            .replace_with(|r| Rc::new(TypeResolver::new(Rc::clone(r))));
        let scope = self.scope.borrow();
        let resolver = self.type_resolver.borrow();
        for (name, ty) in parameters {
            scope.insert(
                name.clone(),
                resolver.resolve(ty).expect("Failed to resolve type"),
            );
        }
        drop(scope);
        let return_type = resolver
            .resolve(return_type)
            .expect("Failed to resolve type");
        drop(resolver);
        let body = self.expression(body);

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

        self.scope.replace(prev_scope);

        let params = parameters
            .into_iter()
            .map(|(_, ty)| {
                self.type_resolver
                    .borrow()
                    .resolve(ty)
                    .expect("Failed to resolve type")
            })
            .collect::<Vec<_>>();

        let parameters = parameters
            .into_iter()
            .zip(params.iter())
            .map(|((name, _), ty)| (name.clone(), ty.clone()))
            .collect();

        let fn_type = TcAstType::Fn {
            return_type: Box::new(return_type.clone()),
            params,
            effects: effects.clone(),
        };

        if self.scope.borrow().insert(name.clone(), fn_type) {
            panic!("Function `{name}` is already defined.");
        }

        let stmt = Statement::FunctionDeclaration {
            name: name.clone(),
            return_type: return_type.clone(),
            parameters,
            body,
            effects: effects.clone(),
        };

        self.type_resolver.replace(prev_resolver);

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
                let prev_resolver = self
                    .type_resolver
                    .replace_with(|r| Rc::new(TypeResolver::new(Rc::clone(r))));

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
                self.type_resolver.replace(prev_resolver);

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

                let operator: TypedBinaryOperator = (*operator).into();

                if !operator.accepts_type(&left_ty, &right_ty) {
                    panic!(
                        "{} cannot be used on types `{:?}` and `{:?}`",
                        *operator, left_ty, right_ty
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
                let ty = self
                    .type_resolver
                    .borrow()
                    .resolve(&literal.ty)
                    .expect("Failed to resolve type");

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
            AstExpression::StructInit {
                use_default,
                ty,
                fields,
            } => {
                let Some(ty) = self.type_resolver.borrow().resolve_alias(match ty {
                    AstType::Custom(val) => val,
                    _ => todo!(),
                }) else {
                    panic!("Struct `{ty:?}` is not present in current scope.");
                };
                let Some(struct_fields) = ty.clone().as_struct() else {
                    panic!("`{ty:?}` is not a struct.");
                };

                if *use_default {
                    todo!("use_default");
                }

                let mut fields = fields
                    .into_iter()
                    .map(|(name, expr)| (name.clone(), self.expression(expr)))
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
                    } else {
                        if field_ty != *expected_ty {
                            panic!(
                                "Field expected expression of type `{expected_ty:?}`, but found `{field_ty:?}`."
                            );
                        }
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
                    .map(|field| field.1.effects.as_slice())
                    .flatten()
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
