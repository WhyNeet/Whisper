pub mod resolver;
pub mod scope;

use std::{cell::RefCell, collections::HashSet, iter, path::PathBuf, rc::Rc, sync::Arc};
use string_cache::DefaultAtom as Atom;

use ast::{
    expr::Expression as AstExpression,
    module::Module as AstModule,
    stmt::{
        FunctionDeclaration, Impl, Import, Namespace, Statement as AstStatement, StructDeclaration,
        VariableDeclaration,
    },
};
use common::{
    effects::Effect,
    module::{ModuleRegistry, ModuleSymbol, SymbolTable, Visibility},
    types::Type as TcAstType,
};
use tcast::{
    expr::{Expression, Literal, TypedExpression},
    module::Module,
    ops::{TypedBinaryOperator, TypedUnaryOperator},
    stmt::{
        FunctionDeclaration as TFunctionDeclaration, Impl as TImpl, Import as TImport,
        Namespace as TNamespace, Statement, StructDeclaration as TStructDeclaration,
        StructField as TStructField, TypedStatement, VariableDeclaration as TVariableDeclaration,
    },
};

use crate::scope::{Scope, ScopeValueData};

#[derive(Default)]
pub struct Checker {
    scope: RefCell<Rc<Scope>>,
    main_fn: RefCell<Option<Atom>>,
    symbol_table: SymbolTable,
    registry: Arc<ModuleRegistry>,
}

impl Checker {
    pub fn new(registry: Arc<ModuleRegistry>) -> Self {
        Self {
            scope: RefCell::new(Rc::new(Scope::root())),
            registry,
            ..Default::default()
        }
    }
}

impl Checker {
    pub fn run(self, module: AstModule) -> Module {
        let mut stmts = vec![];

        for stmt in module.stmts.into_iter() {
            stmts.push(self.statement(stmt));
        }

        Module {
            stmts,
            entrypoint: self.main_fn.take(),
            symbols: self.symbol_table,
        }
    }

    pub fn statement(&self, stmt: AstStatement) -> TypedStatement {
        match stmt {
            AstStatement::Expression(expr) => {
                let expr = self.expression(expr.expr, None);

                TypedStatement {
                    effects: expr.effects.clone(),
                    stmt: Statement::Expression(expr),
                }
            }
            AstStatement::FunctionDeclaration(func) => {
                let name = func.name.clone();
                if &name == "main" {
                    if let Some(main_fn) = &*self.main_fn.borrow() {
                        panic!("Main function is already defined (`{main_fn}`)");
                    }
                    self.main_fn.replace(Some(name.clone()));
                }

                if func.is_extern && func.body.is_some() {
                    panic!("Extern function cannot have a body.");
                }

                if !func.is_extern && func.body.is_none() {
                    panic!("Function must have a body.");
                }

                self.fn_declaration(*func)
            }
            AstStatement::VariableDeclaration(var) => self.var_declaration(*var),
            AstStatement::StructDeclaration(str) => self.struct_declaration(*str),
            AstStatement::Impl(impl_stmt) => self.impl_stmt(*impl_stmt),
            AstStatement::Namespace(namespace) => self.namespace_stmt(*namespace),
            AstStatement::Import(import) => self.import_stmt(*import),
        }
    }

    fn import_stmt(&self, import: Import) -> TypedStatement {
        let Import {
            module_id,
            alias,
            path,
        } = import;
        let module_id = module_id.unwrap();

        let module = self.registry.get(&module_id).unwrap().unwrap();

        let namespace = self.scope.borrow().create_namespace(alias.clone()).unwrap();

        let f = |(name, symbol): (&Atom, &ModuleSymbol)| {
            if symbol.visibility.is_public() {
                namespace.add_member(name.clone(), symbol.ty.clone());
            }
        };

        module.symbols.pairs(Box::new(f));

        TypedStatement {
            stmt: Statement::Import(Box::new(TImport {
                module_id,
                alias,
                path: path.into_iter().fold(PathBuf::new(), |mut acc, val| {
                    acc.push(val.as_ref());
                    acc
                }),
            })),
            effects: vec![],
        }
    }

    fn namespace_stmt(&self, namespace: Namespace) -> TypedStatement {
        let Namespace { name, stmts } = namespace;
        let prev_scope = self.scope.replace_with(|old| Scope::new(Rc::clone(old)));
        let namespace = prev_scope
            .create_namespace(name)
            .expect("Namespace is already declared");

        let stmts = stmts.into_iter().map(|stmt| self.statement(stmt)).collect();

        let tmp_scope = self.scope.replace(prev_scope);
        let tmp_scope = Rc::try_unwrap(tmp_scope).unwrap();
        let values = tmp_scope.unwrap_values();

        for (name, value) in values.borrow().iter() {
            namespace.add_member(name.clone(), value.ty.clone())
        }

        TypedStatement {
            effects: vec![],
            stmt: Statement::Namespace(Box::new(TNamespace { stmts })),
        }
    }

    fn impl_stmt(&self, impl_stmt: Impl) -> TypedStatement {
        let Impl { ident, methods } = impl_stmt;

        let methods = methods
            .into_iter()
            .map(|method| {
                let parameters = method
                    .parameters
                    .into_iter()
                    .map(|(name, ty)| {
                        (
                            name,
                            self.scope
                                .borrow()
                                .type_resolver()
                                .resolve_ast_type(&ty)
                                .expect("Failed to resolve type"),
                        )
                    })
                    .collect::<Vec<_>>();

                TFunctionDeclaration {
                    name: method.name,
                    body: {
                        let prev_scope = self
                            .scope
                            .replace_with(|scope| Scope::new(Rc::clone(scope)));
                        let scope = self.scope.borrow();
                        for (name, ty) in parameters.iter() {
                            scope.insert(name.clone(), ty.clone(), false);
                        }
                        drop(scope);
                        let expr = self.expression(method.body.unwrap(), None);

                        self.scope.replace(prev_scope);

                        Some(expr)
                    },
                    effects: method.effects.clone(),
                    is_pub: method.is_pub,
                    is_extern: false,
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

        let scope = self.scope.borrow();
        let resolver = scope.type_resolver();
        resolver.add_impl(resolver.resolve_alias(&ident).unwrap(), methods.clone());

        let stmt = Statement::Impl(Box::new(TImpl { ident, methods }));

        TypedStatement {
            effects: vec![],
            stmt,
        }
    }

    pub fn struct_declaration(&self, str: StructDeclaration) -> TypedStatement {
        let StructDeclaration {
            name,
            fields,
            is_pub,
        } = str;

        let scope = self.scope.borrow();
        let resolver = scope.type_resolver();
        let fields = fields
            .into_iter()
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

        if self
            .scope
            .borrow()
            .type_resolver()
            .insert(name.clone(), ty.clone())
        {
            panic!("Struct `{name}` is already defined in current scope.");
        }

        if self.scope.borrow().is_root() {
            self.symbol_table.insert(
                name.clone(),
                ModuleSymbol {
                    visibility: if is_pub {
                        Visibility::Public
                    } else {
                        Visibility::Private
                    },
                    ty,
                },
            );
        }

        let stmt = Statement::StructDeclaration(Box::new(TStructDeclaration {
            name,
            fields: fields.clone(),
            is_pub,
        }));

        TypedStatement {
            effects: vec![],
            stmt,
        }
    }

    pub fn var_declaration(&self, var: VariableDeclaration) -> TypedStatement {
        let VariableDeclaration {
            name,
            is_mut,
            expr,
            ty,
        } = var;

        let expr = self.expression(
            expr,
            ty.as_ref()
                .and_then(|ty| self.scope.borrow().type_resolver().resolve_ast_type(ty))
                .as_ref(),
        );

        self.scope
            .borrow()
            .insert(name.clone(), expr.ty.clone(), is_mut);

        let expr_effects = expr.effects.clone();

        let stmt =
            Statement::VariableDeclaration(Box::new(TVariableDeclaration { name, is_mut, expr }));

        TypedStatement {
            effects: expr_effects,
            stmt,
        }
    }

    pub fn fn_declaration(&self, func: FunctionDeclaration) -> TypedStatement {
        let FunctionDeclaration {
            name,
            return_type,
            parameters,
            body,
            effects,
            is_extern,
            is_pub,
        } = func;

        let prev_scope = self
            .scope
            .replace_with(|scope| Scope::new(Rc::clone(scope)));
        let scope = self.scope.borrow();
        let resolver = prev_scope.type_resolver();
        for (name, ty) in parameters.iter() {
            scope.insert(
                name.clone(),
                resolver
                    .resolve_ast_type(ty)
                    .unwrap_or_else(|| panic!("Failed to resolve type: {ty:?}")),
                false,
            );
        }
        let return_type = resolver
            .resolve_ast_type(&return_type)
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
            .into_iter()
            .zip(params.iter())
            .map(|((name, _), ty)| (name, ty.clone()))
            .collect();

        let fn_type = TcAstType::Fn {
            return_type: Box::new(return_type.clone()),
            params,
            effects: effects.to_owned(),
        };

        if self
            .scope
            .borrow()
            .insert(name.clone(), fn_type.clone(), false)
        {
            panic!("Function `{name}` is already defined.");
        }

        if self.scope.borrow().is_root() {
            self.symbol_table.insert(
                name.clone(),
                ModuleSymbol {
                    ty: fn_type,
                    visibility: if is_pub {
                        Visibility::Public
                    } else {
                        Visibility::Private
                    },
                },
            );
        }

        let stmt = Statement::FunctionDeclaration(Box::new(TFunctionDeclaration {
            name: name.clone(),
            return_type,
            parameters,
            body,
            is_extern,
            effects: effects.to_owned(),
            is_pub,
        }));

        TypedStatement {
            effects: effects.to_owned(),
            stmt,
        }
    }

    pub fn expression(
        &self,
        expr: AstExpression,
        expect_ty: Option<&TcAstType>,
    ) -> TypedExpression {
        match expr {
            AstExpression::Block { stmts, return_expr } => {
                let prev_scope = self
                    .scope
                    .replace_with(|scope| Scope::new(Rc::clone(scope)));

                let stmts = stmts
                    .into_iter()
                    .map(|stmt| self.statement(stmt))
                    .collect::<Vec<_>>();
                let return_expr = return_expr
                    .map(|expr| self.expression(*expr, None))
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
                    is_mut: false,
                }
            }
            AstExpression::Binary {
                operator,
                left,
                right,
            } => {
                let left = self.expression(*left, None);
                let right = self.expression(*right, None);

                let effects = [left.effects.as_slice(), right.effects.as_slice()].concat();

                let operator: TypedBinaryOperator = operator.into();

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
                    is_mut: false,
                }
            }
            AstExpression::Unary { operator, expr } => {
                let expr = self.expression(*expr, None);

                let effects = expr.effects.clone();

                let operator: TypedUnaryOperator = operator.into();

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
                    is_mut: false,
                }
            }
            AstExpression::Grouping(expr) => {
                let expr = self.expression(*expr, expect_ty);
                let is_mut = expr.is_mut;
                let expr_ty = expr.ty.clone();
                let effects = expr.effects.clone();

                let expr = Expression::Grouping(Box::new(expr));

                TypedExpression {
                    expr,
                    effects,
                    ty: expr_ty,
                    is_mut,
                }
            }
            AstExpression::Literal(literal) => {
                let ty = expect_ty
                    .and_then(|ty| {
                        if ty.is_whole() && literal.ty.is_whole() {
                            Some(ty.clone())
                        } else {
                            None
                        }
                    })
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
                    is_mut: false,
                }
            }
            AstExpression::Identifier(ident) => {
                let scope = self.scope.borrow();
                let resolver = scope.type_resolver();
                if let Some(data) = scope.get(&ident) {
                    TypedExpression {
                        expr: Expression::Identifier(ident),
                        ty: data.ty,
                        effects: vec![],
                        is_mut: data.is_mut,
                    }
                } else if let Some(namespace) = scope.get_namespace(&ident) {
                    TypedExpression {
                        expr: Expression::Identifier(ident.clone()),
                        ty: TcAstType::Namespace {
                            alias: ident,
                            fields: namespace.take_members(),
                        },
                        effects: vec![],
                        is_mut: false,
                    }
                } else if let Some(ty) = resolver.resolve_alias(&ident) {
                    TypedExpression {
                        expr: Expression::Identifier(ident),
                        ty,
                        effects: vec![],
                        is_mut: false,
                    }
                } else {
                    panic!("`{ident}` is not present in current scope.")
                }
            }
            AstExpression::FunctionCall { expr, args } => {
                let expr = self.expression(*expr, None);

                let Some((fn_return_type, fn_params, fn_effects)) = expr.ty.clone().as_fn() else {
                    panic!("`{expr:?}` is not callable.")
                };

                if args.len() != fn_params.len() {
                    panic!(
                        "Invalid number of arguments: expected {}, found {}",
                        args.len(),
                        fn_params.len()
                    );
                }
                let args = args
                    .into_iter()
                    .zip(fn_params.iter())
                    .map(|(arg, ty)| self.expression(arg, Some(ty)))
                    .collect::<Vec<_>>();

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
                    is_mut: false,
                }
            }
            AstExpression::MemberAccess { expr, ident } => {
                let expr = self.expression(*expr, None);
                let is_mut = expr.is_mut;
                if let Some((_, fields)) = expr
                    .ty
                    .clone()
                    .as_struct()
                    .or_else(|| expr.ty.clone().as_namespace())
                {
                    let scope = self.scope.borrow();

                    let Some(data) = fields
                        .into_iter()
                        .find(|(name, _)| *name == ident)
                        .map(|(_, ty)| ScopeValueData { ty, is_mut })
                        .or_else(|| scope.get(&ident))
                    else {
                        panic!("Expression does not contain member `{ident}`");
                    };

                    let expr = Expression::MemberAccess {
                        expr: Box::new(expr),
                        ident: ident.clone(),
                    };

                    TypedExpression {
                        ty: data.ty,
                        effects: vec![],
                        expr,
                        is_mut,
                    }
                } else {
                    panic!("Expression does not contain member `{ident}`");
                }
            }
            AstExpression::MethodAccess { expr, ident } => {
                let expr = self.expression(*expr, None);

                let impls = self
                    .scope
                    .borrow()
                    .type_resolver()
                    .resolve_impl(&expr.ty)
                    .unwrap_or_else(|| panic!("No method `{ident}` found"));

                let method = impls
                    .into_iter()
                    .flat_map(|i| i.methods)
                    .find(|method| method.name == *ident)
                    .unwrap_or_else(|| panic!("No method `{ident}` found"));

                TypedExpression {
                    ty: TcAstType::Fn {
                        return_type: Box::new(method.return_type),
                        params: method.parameters.into_iter().map(|(_, ty)| ty).collect(),
                        effects: method.effects,
                    },
                    effects: vec![],
                    expr: Expression::MethodAccess {
                        expr: Box::new(expr),
                        ident: ident.clone(),
                    },
                    is_mut: false,
                }
            }
            AstExpression::StructInit {
                use_default,
                ty,
                fields,
            } => {
                let Some(ty) = self.scope.borrow().type_resolver().resolve_ast_type(&ty) else {
                    panic!("Struct `{ty:?}` is not present in current scope.");
                };
                let Some((_, struct_fields)) = ty.clone().as_struct() else {
                    panic!("`{ty:?}` is not a struct.");
                };

                if use_default {
                    todo!("use_default");
                }

                let mut fields = fields
                    .into_iter()
                    .map(|(name, expr)| {
                        (
                            name.clone(),
                            self.expression(
                                expr,
                                Some(
                                    struct_fields
                                        .iter()
                                        .find(|(ident, _)| *ident == name)
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
                    use_default,
                    ty: ty.clone(),
                    fields: sorted_fields,
                };

                TypedExpression {
                    ty,
                    effects,
                    expr,
                    is_mut: false,
                }
            }
            AstExpression::Assignment { assignee, expr } => {
                let assignee = self.expression(*assignee, None);
                if !assignee.is_mut {
                    panic!("Expression cannot be mutated");
                }
                let expr = self.expression(*expr, Some(&assignee.ty));

                let expr = Expression::Assignment {
                    assignee: Box::new(assignee),
                    expr: Box::new(expr),
                };

                TypedExpression {
                    ty: TcAstType::Unit,
                    effects: vec![Effect::Mut],
                    expr,
                    is_mut: false,
                }
            }
        }
    }
}

// if let Some(impls) = impls {
//     impls
//         .and_then(|impls| {
//             impls.into_iter().find_map(|i| {
//                 i.methods.into_iter().find(|i| i.name == *ident).map(
//                     |method| {
//                         (
//                             method.name,
//                             TcAstType::Fn {
//                                 return_type: Box::new(method.return_type),
//                                 params: method
//                                     .parameters
//                                     .into_iter()
//                                     .map(|(_, ty)| ty)
//                                     .collect(),
//                                 effects: method.effects,
//                             },
//                         )
//                     },
//                 )
//             })
//         })
//         .map(|(_, ty)| ty)
// } else {
//     scope.get(ident)
// }
