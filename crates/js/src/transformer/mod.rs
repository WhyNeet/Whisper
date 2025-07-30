pub mod resolver;

use common::{
    literal::LiteralValue, module::ModuleRegistry, ops::UnaryOperator as LangUnaryOperator,
    types::Type,
};
use std::{cell::RefCell, path::PathBuf, rc::Rc, sync::Arc};
use string_cache::DefaultAtom as Atom;
use tcast::{
    expr::{Expression as TExpression, TypedExpression},
    module::Module,
    stmt::{
        FunctionDeclaration, Import, Statement as TStatement, StructDeclaration, TypedStatement,
        VariableDeclaration,
    },
};

use crate::{
    ast::{
        expr::Expression,
        literal::Literal,
        ops::{BinaryOperator, UnaryOperator},
        program::Program,
        stmt::{Import as JsImport, Statement},
    },
    transformer::resolver::TypeResolver,
};

#[derive(Debug)]
pub struct TypedAstTransformer {
    type_resolver: RefCell<Rc<TypeResolver>>,
    registry: Arc<ModuleRegistry>,
}

impl TypedAstTransformer {
    pub fn new(registry: Arc<ModuleRegistry>) -> Self {
        Self {
            type_resolver: Default::default(),
            registry,
        }
    }

    pub fn run(&self, module: Module) -> Program {
        let mut stmts = vec![];

        for stmt in module.stmts {
            stmts.append(&mut self.statement(stmt));
        }

        if let Some(ref entrypoint) = module.entrypoint {
            stmts.push(Statement::Expression(Expression::FunctionCall {
                expr: Box::new(Expression::Identifier(entrypoint.clone())),
                args: vec![],
            }));
        }

        Program { stmts }
    }

    fn statement(&self, stmt: TypedStatement) -> Vec<Statement> {
        match stmt.stmt {
            TStatement::Expression(expr) => {
                if stmt.effects.is_empty() {
                    return vec![];
                }

                let (mut tail, expr) = self.expression(expr);
                tail.push(Statement::Expression(expr));

                tail
            }
            TStatement::FunctionDeclaration(func) => {
                if func.body.is_some() {
                    vec![self.fn_declaration(*func)]
                } else {
                    vec![]
                }
            }
            TStatement::VariableDeclaration(var) => self.var_declaration(*var),
            TStatement::StructDeclaration(str) => {
                self.type_resolver.borrow().insert(
                    str.name.clone(),
                    Type::Struct {
                        fields: str
                            .fields
                            .iter()
                            .map(|field| (field.name.clone(), field.ty.clone()))
                            .collect(),
                        alias: str.name.clone(),
                    },
                );
                vec![self.struct_declaration(*str)]
            }
            TStatement::Impl(impl_stmt) => impl_stmt
                .methods
                .into_iter()
                .map(|method| {
                    self.fn_declaration(FunctionDeclaration {
                        name: Atom::from(format!("{}_{}", impl_stmt.ident, method.name)),
                        ..method
                    })
                })
                .collect(),
            TStatement::Namespace { .. } => vec![],
            TStatement::Import(import) => vec![self.import(*import)],
        }
    }

    fn import(&self, import: Import) -> Statement {
        let Import { module_id } = import;

        let module = self.registry.get(&module_id).unwrap().unwrap();

        let path = module
            .module_path
            .iter()
            .fold(PathBuf::new(), |mut acc, val| {
                acc.push(val.as_ref());
                acc
            });

        Statement::Import(Box::new(JsImport {
            name: module.name.clone(),
            path: Atom::from(path.to_str().unwrap()),
        }))
    }

    fn struct_declaration(&self, str: StructDeclaration) -> Statement {
        let StructDeclaration {
            name,
            fields,
            is_pub,
        } = str;
        let body = fields
            .iter()
            .map(|field| Statement::Assignment {
                target: Expression::MemberAccess {
                    expr: Box::new(Expression::Identifier(Atom::from("this"))),
                    ident: field.name.clone(),
                },
                expr: Expression::Identifier(field.name.clone()),
            })
            .collect();

        let stmt = Statement::FunctionDeclaration {
            is_async: false,
            ident: name,
            params: fields.iter().map(|field| field.name.clone()).collect(),
            body,
        };

        if is_pub {
            Statement::Export(Box::new(stmt))
        } else {
            stmt
        }
    }

    fn var_declaration(&self, var: VariableDeclaration) -> Vec<Statement> {
        let VariableDeclaration { name, is_mut, expr } = var;

        let (mut tail, expr) = self.expression(expr);

        tail.push(Statement::VariableDeclaration {
            is_const: !is_mut,
            ident: name,
            expression: expr,
        });

        tail
    }

    fn fn_declaration(&self, func: FunctionDeclaration) -> Statement {
        let FunctionDeclaration {
            name,
            parameters,
            body,
            is_pub,
            ..
        } = func;
        let body = body.unwrap();
        let (stmts, ret) = self.expression(body);
        let mut body = stmts;
        if ret != Expression::Literal(Literal::Void) {
            body.push(Statement::Return(ret));
        }

        let stmt = Statement::FunctionDeclaration {
            is_async: false,
            ident: name,
            params: parameters.iter().map(|(param, _)| param.clone()).collect(),
            body,
        };

        if is_pub {
            Statement::Export(Box::new(stmt))
        } else {
            stmt
        }
    }

    fn expression(&self, expr: TypedExpression) -> (Vec<Statement>, Expression) {
        match expr.expr {
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
            TExpression::Identifier(ident) => (vec![], Expression::Identifier(ident.clone())),
            TExpression::Grouping(expr) => {
                let (tail, expr) = self.expression(*expr);
                (tail, Expression::Grouping(Box::new(expr)))
            }
            TExpression::Unary { operator, expr } => {
                let (tail, expr) = self.expression(*expr);

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
                let (left_tail, left) = self.expression(*left);
                let (right_tail, right) = self.expression(*right);

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
                let (tail, expr) = self.expression(*expr);

                (
                    tail,
                    Expression::MemberAccess {
                        expr: Box::new(expr),
                        ident: ident.clone(),
                    },
                )
            }
            TExpression::MethodAccess {
                expr,
                ident: method_ident,
            } => {
                let (tail, expr) = self.expression(*expr);

                (
                    tail,
                    Expression::MethodAccess {
                        expr: Box::new(expr),
                        ident: method_ident.clone(),
                    },
                )
            }
            TExpression::FunctionCall { expr, args } => {
                let (tail, expr) = self.expression(*expr);

                let (args_tails, args) = args
                    .into_iter()
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
                    .into_iter()
                    .map(|stmt| self.statement(stmt))
                    .collect::<Vec<_>>();
                let return_expr = return_expr.map(|expr| self.expression(*expr));

                let Some((ret_tail, expr)) = return_expr else {
                    return (tail.concat(), Expression::Literal(Literal::Void));
                };

                tail.push(ret_tail);

                (tail.concat(), expr)
            }
            TExpression::StructInit { ty, fields, .. } => {
                let (tails, exprs) = fields
                    .into_iter()
                    .map(|(_, expr)| {
                        let (tail, expr) = self.expression(expr);
                        (tail, expr)
                    })
                    .collect::<(Vec<_>, Vec<_>)>();
                let tail = tails.concat();

                let name = self.type_resolver.borrow().resolve(&ty).unwrap();

                let expr = Expression::New {
                    expr: Box::new(Expression::FunctionCall {
                        expr: Box::new(Expression::Identifier(name)),
                        args: exprs,
                    }),
                };

                (tail, expr)
            }
            TExpression::Assignment { assignee, expr } => {
                let (mut assignee_tail, assignee_expr) = self.expression(*assignee);
                let (mut tail, expr) = self.expression(*expr);

                assignee_tail.append(&mut tail);
                let tail = assignee_tail;

                (
                    tail,
                    Expression::Assignment {
                        assignee: Box::new(assignee_expr),
                        expr: Box::new(expr),
                    },
                )
            }
            TExpression::Namespace { alias, .. } => (vec![], Expression::Identifier(alias.clone())),
        }
    }
}
