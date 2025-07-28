use ast::expr::{Expression, Literal};
use ast::module::Module;
use ast::stmt::{Statement, StructField, StructMethod};
use ast::types::Type;
use common::effects::Effect;
use common::literal::LiteralValue;
use common::ops::{BinaryOperator, UnaryOperator};
use core::unreachable;
use lexer::stream::TokenStream;
use lexer::token::{Keyword, LiteralKind, Token, TokenKind};
use std::mem;
use string_cache::DefaultAtom as Atom;

pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(token_stream: TokenStream<'a>) -> Self {
        Self { token_stream }
    }

    pub fn run(&mut self) -> Module {
        let mut stmts = vec![];

        while let Some(token) = self.token_stream.peek() {
            match token.kind {
                TokenKind::LineComment => continue,
                TokenKind::BlockComment { terminated } => {
                    if !terminated {
                        panic!("Unterminated block comment.")
                    }
                    continue;
                }
                _ => (),
            }

            stmts.push(self.statement());
        }

        Module { stmts }
    }

    fn statement(&mut self) -> Statement {
        if self.matches(TokenKind::Keyword(Keyword::Fn)).is_some() {
            self.fn_decl(false, false)
        } else if self
            .matches(TokenKind::Keyword(Keyword::Extern))
            .and_then(|_| self.matches(TokenKind::Keyword(Keyword::Fn)))
            .is_some()
        {
            self.fn_decl(true, false)
        } else if self.matches(TokenKind::Keyword(Keyword::Pub)).is_some() {
            if self.matches(TokenKind::Keyword(Keyword::Fn)).is_some() {
                self.fn_decl(false, true)
            } else if self
                .matches(TokenKind::Keyword(Keyword::Extern))
                .and_then(|_| self.matches(TokenKind::Keyword(Keyword::Fn)))
                .is_some()
            {
                self.fn_decl(true, true)
            } else if self.matches(TokenKind::Keyword(Keyword::Struct)).is_some() {
                self.struct_decl(true)
            } else {
                panic!("`pub` keyword can only be applied to function or struct declaration.")
            }
        }
        // else if self.matches_peek(TokenKind::At).is_some() {
        //     while self.matches(TokenKind::At).is_some() {
        //         let annotation = self.matches(TokenKind::Ident).expect("Expected identifier");
        //         annotations.push(
        //             self.token_stream.source()[annotation.start..annotation.end]
        //                 .try_into()
        //                 .unwrap(),
        //         );
        //     }

        //     let is_extern = self.matches(TokenKind::Keyword(Keyword::Extern)).is_some();

        //     self.matches(TokenKind::Keyword(Keyword::Fn))
        //         .expect("Expected `fn` keyword");

        //     self.fn_decl(is_extern)
        // }
        else if self.matches(TokenKind::Keyword(Keyword::Let)).is_some() {
            self.var_decl()
        } else if self.matches(TokenKind::Keyword(Keyword::Struct)).is_some() {
            self.struct_decl(false)
        } else if self.matches(TokenKind::Keyword(Keyword::Impl)).is_some() {
            self.impl_stmt()
        } else if self
            .matches(TokenKind::Keyword(Keyword::Namespace))
            .is_some()
        {
            self.namespace_stmt()
        } else {
            self.expr_stmt()
        }
    }

    fn namespace_stmt(&mut self) -> Statement {
        let name = self
            .matches(TokenKind::Ident)
            .expect("Expected namespace identifier");
        let name = Atom::from(&self.token_stream.source()[name.start..name.end]);

        self.matches(TokenKind::OpenBrace)
            .expect("Expected opening brace");

        let mut stmts = vec![];

        while self.matches(TokenKind::CloseBrace).is_none() {
            stmts.push(self.statement());
        }

        Statement::Namespace { name, stmts }
    }

    fn impl_stmt(&mut self) -> Statement {
        let ident = self
            .matches(TokenKind::Ident)
            .expect("Expected struct identifier");
        let ident = Atom::from(&self.token_stream.source()[ident.start..ident.end]);

        self.matches(TokenKind::OpenBrace)
            .expect("Expected opening brace");

        let mut methods = vec![];

        while self.matches(TokenKind::CloseBrace).is_none() {
            let is_pub = self.matches(TokenKind::Keyword(Keyword::Pub)).is_some();

            self.matches(TokenKind::Keyword(Keyword::Fn))
                .expect("Expected `fn` keyword");

            let effects = self.effects();

            let name = self.matches(TokenKind::Ident).expect("Expected identifier");
            let name = Atom::from(&self.token_stream.source()[name.start..name.end]);

            self.matches(TokenKind::OpenParen)
                .expect("Expected opening parenthesis");
            let parameters = self.fn_params();
            self.matches(TokenKind::CloseParen)
                .expect("Expected closing parenthesis.");

            let return_type = if self.matches(TokenKind::RArrow).is_some() {
                let ty = self.matches(TokenKind::Ident).expect("Expected type");
                Atom::from(&self.token_stream.source()[ty.start..ty.end]).into()
            } else {
                Type::from(Atom::from("Unit"))
            };

            self.matches(TokenKind::Eq)
                .expect("Expected `=` before function body");

            let body = self.expression();

            let method = StructMethod {
                name,
                is_pub,
                annotations: vec![],
                effects,
                parameters,
                return_type,
                body,
            };

            methods.push(method);
        }

        Statement::Impl { ident, methods }
    }

    fn struct_decl(&mut self, is_pub: bool) -> Statement {
        let ident = self.matches(TokenKind::Ident).expect("Expected identifier");
        let ident = Atom::from(&self.token_stream.source()[ident.start..ident.end]);

        self.matches(TokenKind::OpenBrace)
            .expect("Expected opening brace");

        let mut fields = vec![];

        while self.matches(TokenKind::CloseBrace).is_none() {
            let is_pub = self.matches(TokenKind::Keyword(Keyword::Pub)).is_some();

            let name = self.matches(TokenKind::Ident).expect("Expected identifier");
            let name = Atom::from(&self.token_stream.source()[name.start..name.end]);

            self.matches(TokenKind::Colon).expect("Expected colon");

            let ty = self
                .matches(TokenKind::Ident)
                .expect("Expected type identifier");
            let ty = Atom::from(&self.token_stream.source()[ty.start..ty.end]).into();

            self.matches(TokenKind::Semi).expect("Expected semicolon");

            fields.push(StructField { is_pub, name, ty });
        }

        Statement::StructDeclaration {
            name: ident,
            fields,
            is_pub,
        }
    }

    fn var_decl(&mut self) -> Statement {
        let is_mut = self.matches(TokenKind::Keyword(Keyword::Mut)).is_some();

        let ident = self.matches(TokenKind::Ident).expect("Expected identifier");
        let ident = Atom::from(&self.token_stream.source()[ident.start..ident.end]);

        let ty = if self.matches(TokenKind::Colon).is_some() {
            let ident = self
                .matches(TokenKind::Ident)
                .expect("Expected type identifier");
            Some(Type::from(Atom::from(
                &self.token_stream.source()[ident.start..ident.end],
            )))
        } else {
            None
        };

        self.matches(TokenKind::Eq).expect("Expected `=`");

        let expr = self.expression();

        self.matches(TokenKind::Semi).expect("Expected semicolon");

        Statement::VariableDeclaration {
            name: ident,
            is_mut,
            expr,
            ty,
        }
    }

    fn effects(&mut self) -> Vec<Effect> {
        if self.matches(TokenKind::OpenBrace).is_some() {
            let mut effects = vec![];

            if self.matches(TokenKind::CloseBrace).is_none() {
                let ident = self.matches(TokenKind::Ident).expect("Expected identifier");
                let effect = self.token_stream.source()[ident.start..ident.end]
                    .try_into()
                    .unwrap();
                effects.push(effect);

                while self.matches(TokenKind::Comma).is_some() {
                    let ident = self.matches(TokenKind::Ident).expect("Expected identifier");
                    let effect = self.token_stream.source()[ident.start..ident.end]
                        .try_into()
                        .unwrap();

                    effects.push(effect);
                }

                self.matches(TokenKind::CloseBrace)
                    .expect("Expected closing brace");
            }

            effects
        } else {
            vec![]
        }
    }

    fn fn_decl(&mut self, is_extern: bool, is_pub: bool) -> Statement {
        let effects = self.effects();

        let name = self.matches(TokenKind::Ident).expect("Expected identifier");
        let name = Atom::from(&self.token_stream.source()[name.start..name.end]);

        self.matches(TokenKind::OpenParen)
            .expect("Expected left parenthesis");

        let params = self.fn_params();
        self.matches(TokenKind::CloseParen)
            .expect("Expected right parenthesis");

        let return_type = if self.matches(TokenKind::RArrow).is_some() {
            let ty = self.matches(TokenKind::Ident).expect("Expected type");
            Atom::from(&self.token_stream.source()[ty.start..ty.end]).into()
        } else {
            Type::from(Atom::from("Unit"))
        };

        let body = self.matches(TokenKind::Eq).map(|_| self.expression());

        self.matches(TokenKind::Semi);

        Statement::FunctionDeclaration {
            name,
            return_type,
            parameters: params,
            body,
            effects,
            is_extern,
            is_pub,
        }
    }

    fn fn_params(&mut self) -> Vec<(Atom, Type)> {
        let mut params = vec![];

        if self.matches_peek(TokenKind::Ident).is_some() {
            params.push(self.fn_param());
        }

        while self.matches(TokenKind::Comma).is_some() {
            params.push(self.fn_param());
        }

        params
    }

    fn fn_param(&mut self) -> (Atom, Type) {
        let identifier = self.matches(TokenKind::Ident).expect("Expected identifier");
        let identifier = Atom::from(&self.token_stream.source()[identifier.start..identifier.end]);

        self.matches(TokenKind::Colon).expect("Expected colon");

        let ty = self.matches(TokenKind::Ident).expect("Expected type");
        let ty = Atom::from(&self.token_stream.source()[ty.start..ty.end]).into();
        (identifier, ty)
    }

    fn expr_stmt(&mut self) -> Statement {
        let expr = self.expression();

        Statement::Expression {
            expr,
            has_semi: self.matches(TokenKind::Semi).is_some(),
        }
    }

    fn expression(&mut self) -> Expression {
        self.assignment()
    }

    fn assignment(&mut self) -> Expression {
        let mut expr = self.term();

        if self.matches(TokenKind::Eq).is_some() {
            let assigned_expr = self.expression();
            expr = Expression::Assignment {
                assignee: Box::new(expr),
                expr: Box::new(assigned_expr),
            };
        }

        expr
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        while let Some(token) = self
            .matches(TokenKind::Plus)
            .or_else(|| self.matches(TokenKind::Minus))
        {
            let operator = match token.kind {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Sub,
                _ => unreachable!(),
            };
            let right = self.unary();
            expr = Expression::Binary {
                operator,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        expr
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();

        while let Some(token) = self
            .matches(TokenKind::Star)
            .or_else(|| self.matches(TokenKind::Slash))
        {
            let operator = match token.kind {
                TokenKind::Star => BinaryOperator::Mul,
                TokenKind::Slash => BinaryOperator::Div,
                _ => unreachable!(),
            };
            let right = self.unary();
            expr = Expression::Binary {
                operator,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        expr
    }

    fn unary(&mut self) -> Expression {
        if let Some(token) = self
            .matches(TokenKind::Bang)
            .or_else(|| self.matches(TokenKind::Minus))
        {
            Expression::Unary {
                operator: match token.kind {
                    TokenKind::Bang => UnaryOperator::Not,
                    TokenKind::Minus => UnaryOperator::Neg,
                    _ => unreachable!(),
                },
                expr: Box::new(self.fn_call()),
            }
        } else {
            self.fn_call()
        }
    }

    fn fn_call(&mut self) -> Expression {
        let expr = self.member_access();

        if self.matches(TokenKind::OpenParen).is_some() {
            let mut args = vec![];

            if self.matches(TokenKind::CloseParen).is_none() {
                let arg = self.expression();
                args.push(arg);

                while self.matches(TokenKind::Comma).is_some() {
                    args.push(self.expression());
                }

                self.matches(TokenKind::CloseParen)
                    .expect("Expected closing parenthesis");
            }

            Expression::FunctionCall {
                expr: Box::new(expr),
                args,
            }
        } else {
            expr
        }
    }

    fn member_access(&mut self) -> Expression {
        let mut expr = self.primary();

        while self.matches(TokenKind::Dot).is_some() {
            let ident = self.matches(TokenKind::Ident).expect("Expected identifier");
            let ident = Atom::from(&self.token_stream.source()[ident.start..ident.end]);

            expr = Expression::MemberAccess {
                expr: Box::new(expr),
                ident,
            };
        }

        expr
    }

    fn primary(&mut self) -> Expression {
        if self.matches(TokenKind::OpenParen).is_some() {
            self.grouping()
        } else if let Some(token) = self.matches_kind(TokenKind::Literal {
            kind: LiteralKind::Bool,
        }) {
            let literal = match token.kind {
                TokenKind::Literal { kind } => match kind {
                    LiteralKind::Bool => {
                        let lexeme = &self.token_stream.source()[token.start..token.end];
                        Literal {
                            ty: Type::from(Atom::from("Bool")),
                            value: LiteralValue::Bool(lexeme == "true"),
                        }
                    }
                    LiteralKind::Str { terminated } => {
                        if !terminated {
                            panic!("Unterminated string.");
                        }

                        Literal {
                            ty: Type::from(Atom::from("String")),
                            value: LiteralValue::String(
                                self.token_stream.source()[token.start..token.end].to_string(),
                            ),
                        }
                    }
                    LiteralKind::Char { .. } => todo!(),
                    LiteralKind::Int { empty_int, .. } => {
                        if empty_int {
                            panic!("Empty integer.");
                        }

                        Literal {
                            ty: Type::from(Atom::from("Int")),
                            value: LiteralValue::Integer(
                                self.token_stream.source()[token.start..token.end]
                                    .parse()
                                    .unwrap(),
                            ),
                        }
                    }
                    LiteralKind::Float { .. } => Literal {
                        ty: Type::from(Atom::from("Float")),
                        value: LiteralValue::Float(
                            self.token_stream.source()[token.start..token.end]
                                .parse()
                                .unwrap(),
                        ),
                    },
                },
                _ => unreachable!(),
            };

            Expression::Literal(literal)
        } else if self.matches(TokenKind::OpenBrace).is_some() {
            self.block()
        } else if self.matches(TokenKind::Keyword(Keyword::Default)).is_some() {
            let name = self
                .matches(TokenKind::Ident)
                .expect("Expected struct identifier");
            self.struct_init(name, true)
        } else if let Some(token) = self.matches(TokenKind::Ident) {
            if self.matches_peek(TokenKind::OpenBrace).is_some() {
                self.struct_init(token, false)
            } else if self.matches_peek(TokenKind::ColonColon).is_some() {
                let ident = Atom::from(&self.token_stream.source()[token.start..token.end]);
                let mut expr = Expression::Identifier(ident);

                while self.matches(TokenKind::ColonColon).is_some() {
                    let token = self.matches(TokenKind::Ident).expect("Expected identifier");
                    let ident = Atom::from(&self.token_stream.source()[token.start..token.end]);
                    expr = Expression::MethodAccess {
                        expr: Box::new(expr),
                        ident,
                    };
                }

                expr
            } else {
                let ident = Atom::from(&self.token_stream.source()[token.start..token.end]);
                Expression::Identifier(ident)
            }
        } else {
            todo!("{:?}", self.token_stream.peek())
        }
    }

    fn struct_init(&mut self, name: Token, use_default: bool) -> Expression {
        self.matches(TokenKind::OpenBrace)
            .expect("Expected opening brace");

        let mut fields = vec![];

        if self.matches(TokenKind::CloseBrace).is_none() {
            while self.matches(TokenKind::CloseBrace).is_none() {
                let name = self.matches(TokenKind::Ident).expect("Expected field name");
                let name = Atom::from(&self.token_stream.source()[name.start..name.end]);
                self.matches(TokenKind::Colon).expect("Expected colon");
                let expr = self.expression();

                if self.matches(TokenKind::Comma).is_none()
                    && self.matches_peek(TokenKind::CloseBrace).is_none()
                {
                    panic!("Expected comma.")
                }

                fields.push((name, expr));
            }
        }

        Expression::StructInit {
            use_default,
            ty: Atom::from(&self.token_stream.source()[name.start..name.end]).into(),
            fields,
        }
    }

    fn grouping(&mut self) -> Expression {
        let expr = self.expression();
        self.matches(TokenKind::CloseParen)
            .expect("Expected closing parenthesis");
        Expression::Grouping(Box::new(expr))
    }

    fn block(&mut self) -> Expression {
        let mut stmts = vec![];
        while self.matches(TokenKind::CloseBrace).is_none() {
            match self.token_stream.peek().unwrap().kind {
                TokenKind::LineComment => {
                    self.token_stream.next();
                    continue;
                }
                TokenKind::BlockComment { terminated } => {
                    if !terminated {
                        panic!("Unterminated block comment.")
                    }
                    self.token_stream.next();
                    continue;
                }
                _ => (),
            }

            let stmt = self.statement();
            stmts.push(stmt);
        }

        if !stmts.is_empty()
            && !stmts[..stmts.len() - 1].iter().all(|stmt| match stmt {
                Statement::Expression { has_semi, .. } => *has_semi,
                _ => true,
            })
        {
            panic!("Expected semicolon");
        }

        if !stmts.is_empty()
            && match stmts.last().unwrap() {
                Statement::Expression { has_semi, .. } => !has_semi,
                _ => false,
            }
        {
            let last = match stmts.pop().unwrap() {
                Statement::Expression { expr, .. } => expr,
                _ => unreachable!(),
            };
            Expression::Block {
                stmts,
                return_expr: Some(Box::new(last)),
            }
        } else {
            Expression::Block {
                stmts,
                return_expr: None,
            }
        }
    }
}

impl Parser<'_> {
    fn matches(&mut self, kind: TokenKind) -> Option<Token> {
        if let Some(token) = self.token_stream.peek() {
            if token.kind == kind {
                self.token_stream.next()
            } else {
                None
            }
        } else {
            None
        }
    }

    fn matches_peek(&mut self, kind: TokenKind) -> Option<&Token> {
        self.token_stream.peek().filter(|&token| token.kind == kind)
    }

    fn matches_kind(&mut self, kind: TokenKind) -> Option<Token> {
        if let Some(token) = self.token_stream.peek() {
            if mem::discriminant(&token.kind) == mem::discriminant(&kind) {
                self.token_stream.next()
            } else {
                None
            }
        } else {
            None
        }
    }
}
