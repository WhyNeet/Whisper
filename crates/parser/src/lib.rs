use ast::expr::{Expression, Literal};
use ast::module::Module;
use ast::stmt::{Statement, StructField};
use ast::types::Type;
use common::annotations::Annotation;
use common::literal::LiteralValue;
use common::ops::{BinaryOperator, UnaryOperator};
use core::unreachable;
use lexer::stream::TokenStream;
use lexer::token::{Keyword, LiteralKind, Token, TokenKind};
use std::mem;

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
            self.fn_decl(vec![])
        } else if self.matches_peek(TokenKind::At).is_some() {
            let mut annotations = vec![];

            while self.matches(TokenKind::At).is_some() {
                let annotation = self.matches(TokenKind::Ident).expect("Expected identifier");
                annotations.push(
                    self.token_stream.source()[annotation.start..annotation.end]
                        .try_into()
                        .unwrap(),
                );
            }

            self.matches(TokenKind::Keyword(Keyword::Fn))
                .expect("Expected `fn` keyword");

            self.fn_decl(annotations)
        } else if self.matches(TokenKind::Keyword(Keyword::Let)).is_some() {
            self.var_decl()
        } else if self.matches(TokenKind::Keyword(Keyword::Struct)).is_some() {
            self.struct_decl()
        } else {
            self.expr_stmt()
        }
    }

    fn struct_decl(&mut self) -> Statement {
        let ident = self.matches(TokenKind::Ident).expect("Expected identifier");
        let ident = self.token_stream.source()[ident.start..ident.end].to_string();

        self.matches(TokenKind::OpenBrace)
            .expect("Expected opening brace");

        let mut fields = vec![];

        while !self.matches(TokenKind::CloseBrace).is_some() {
            let is_pub = self.matches(TokenKind::Keyword(Keyword::Pub)).is_some();

            let name = self.matches(TokenKind::Ident).expect("Expected identifier");
            let name = self.token_stream.source()[name.start..name.end].to_string();

            self.matches(TokenKind::Colon).expect("Expected colon");

            let ty = self
                .matches(TokenKind::Ident)
                .expect("Expected type identifier");
            let ty = self.token_stream.source()[ty.start..ty.end]
                .try_into()
                .unwrap();

            self.matches(TokenKind::Semi).expect("Expected semicolon");

            fields.push(StructField { is_pub, name, ty });
        }

        Statement::StructDeclaration {
            name: ident,
            fields,
        }
    }

    fn var_decl(&mut self) -> Statement {
        let is_mut = self.matches(TokenKind::Keyword(Keyword::Mut)).is_some();

        let ident = self.matches(TokenKind::Ident).expect("Expected identifier");
        let ident = self.token_stream.source()[ident.start..ident.end].to_string();

        self.matches(TokenKind::Eq).expect("Expected `=`");

        let expr = self.expression();

        self.matches(TokenKind::Semi).expect("Expected semicolon");

        Statement::VariableDeclaration {
            name: ident,
            is_mut,
            expr,
        }
    }

    fn fn_decl(&mut self, annotations: Vec<Annotation>) -> Statement {
        let effects = if self.matches(TokenKind::OpenBrace).is_some() {
            let mut effects = vec![];

            if !self.matches(TokenKind::CloseBrace).is_some() {
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
        };

        let name = self.matches(TokenKind::Ident).expect("Expected identifier");
        let name = self.token_stream.source()[name.start..name.end].to_string();

        self.matches(TokenKind::OpenParen)
            .expect("Expected left parenthesis");

        let params = self.fn_params();
        self.matches(TokenKind::CloseParen)
            .expect("Expected right parenthesis");

        let return_type = if self.matches(TokenKind::RArrow).is_some() {
            let ty = self.matches(TokenKind::Ident).expect("Expected type");
            let ty = self.token_stream.source()[ty.start..ty.end]
                .try_into()
                .expect("Invalid type name");
            ty
        } else {
            Type::Unit
        };

        self.matches(TokenKind::Eq)
            .expect("Expected `=` before function body");

        let body = self.expression();

        Statement::FunctionDeclaration {
            name,
            return_type,
            parameters: params,
            body,
            effects,
            annotations,
        }
    }

    fn fn_params(&mut self) -> Vec<(String, Type)> {
        let mut params = vec![];

        if self.matches_peek(TokenKind::Ident).is_some() {
            params.push(self.fn_param());
        }

        while self.matches(TokenKind::Comma).is_some() {
            params.push(self.fn_param());
        }

        params
    }

    fn fn_param(&mut self) -> (String, Type) {
        let identifier = self.matches(TokenKind::Ident).expect("Expected identifier");
        let identifier = self.token_stream.source()[identifier.start..identifier.end].to_string();

        self.matches(TokenKind::Colon).expect("Expected colon");

        let ty = self.matches(TokenKind::Ident).expect("Expected type");
        let ty = self.token_stream.source()[ty.start..ty.end]
            .try_into()
            .expect("Invalid type name");
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
        self.term()
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

            if !self.matches(TokenKind::CloseParen).is_some() {
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
            let ident = self.token_stream.source()[ident.start..ident.end].to_string();

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
                            ty: Type::Bool,
                            value: LiteralValue::Bool(lexeme == "true"),
                        }
                    }
                    LiteralKind::Str { terminated } => {
                        if !terminated {
                            panic!("Unterminated string.");
                        }

                        Literal {
                            ty: Type::String,
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
                            ty: Type::IntN,
                            value: LiteralValue::Integer(
                                self.token_stream.source()[token.start..token.end]
                                    .parse()
                                    .unwrap(),
                            ),
                        }
                    }
                    LiteralKind::Float { .. } => Literal {
                        ty: Type::FloatN,
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
            } else {
                let ident = self.token_stream.source()[token.start..token.end].to_string();
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

        if !self.matches(TokenKind::CloseBrace).is_some() {
            while !self.matches(TokenKind::CloseBrace).is_some() {
                let name = self.matches(TokenKind::Ident).expect("Expected field name");
                let name = self.token_stream.source()[name.start..name.end].to_string();
                self.matches(TokenKind::Colon).expect("Expected colon");
                let expr = self.expression();

                if !self.matches(TokenKind::Comma).is_some()
                    && !self.matches_peek(TokenKind::CloseBrace).is_some()
                {
                    panic!("Expected comma.")
                }

                fields.push((name, expr));
            }
        }

        Expression::StructInit {
            use_default,
            ty: self.token_stream.source()[name.start..name.end].into(),
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
        while !self.matches(TokenKind::CloseBrace).is_some() {
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

impl<'a> Parser<'a> {
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
        if let Some(token) = self.token_stream.peek() {
            if token.kind == kind {
                Some(token)
            } else {
                None
            }
        } else {
            None
        }
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
