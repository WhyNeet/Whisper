use ast::expr::{Expression, Literal};
use ast::ops::{BinaryOperator, UnaryOperator};
use ast::stmt::Statement;
use core::unreachable;
use lexer::stream::TokenStream;
use lexer::token::{LiteralKind, Token, TokenKind};
use std::mem;

pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(token_stream: TokenStream<'a>) -> Self {
        Self { token_stream }
    }

    pub fn run(&mut self) -> Vec<Statement> {
        let mut stmts = vec![];

        while self.token_stream.peek().is_some() {
            stmts.push(self.statement());
        }

        stmts
    }

    fn statement(&mut self) -> Statement {
        self.expr_stmt()
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
                    LiteralKind::Int { base, empty_int } => {
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
