use ast::expr::{Expression, Literal};
use ast::module::Module;
use ast::ops::{BinaryOperator, UnaryOperator};
use ast::stmt::Statement;
use common::types::Type;
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

        while self.token_stream.peek().is_some() {
            stmts.push(self.statement());
        }

        Module { stmts }
    }

    fn statement(&mut self) -> Statement {
        if self.matches(TokenKind::Keyword(Keyword::Fn)).is_some() {
            self.fn_decl()
        } else {
            self.expr_stmt()
        }
    }

    fn fn_decl(&mut self) -> Statement {
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
        self.matches(TokenKind::Semi).expect("Expected semicolon");

        Statement::Expression(expr)
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
                expr: Box::new(self.primary()),
            }
        } else {
            self.primary()
        }
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
                        Literal::Bool(lexeme == "true")
                    }
                    LiteralKind::Str { terminated } => {
                        if !terminated {
                            panic!("Unterminated string.");
                        }

                        Literal::String(
                            self.token_stream.source()[token.start..token.end].to_string(),
                        )
                    }
                    LiteralKind::Int { empty_int, .. } => {
                        if empty_int {
                            panic!("Empty integer.");
                        }

                        Literal::Integer(
                            self.token_stream.source()[token.start..token.end]
                                .parse()
                                .unwrap(),
                        )
                    }
                    _ => todo!(),
                },
                _ => unreachable!(),
            };

            Expression::Literal(literal)
        } else if let Some(token) = self.matches(TokenKind::Ident) {
            let ident = self.token_stream.source()[token.start..token.end].to_string();
            Expression::Identifier(ident)
        } else {
            todo!()
        }
    }

    fn grouping(&mut self) -> Expression {
        let expr = self.expression();
        self.matches(TokenKind::CloseParen)
            .expect("Expected closing parenthesis");
        Expression::Grouping(Box::new(expr))
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
