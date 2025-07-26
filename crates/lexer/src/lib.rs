use crate::{
    cursor::Cursor,
    token::{Base, Keyword, LiteralKind, Token, TokenKind},
};

pub mod cursor;
pub mod stream;
pub mod token;

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.next_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

impl Cursor<'_> {
    pub fn next_token(&mut self) -> Token {
        let start = self.consumed();
        let first_char = match self.bump() {
            Some(c) => c,
            None => {
                return Token {
                    kind: TokenKind::Eof,
                    start: self.consumed(),
                    end: self.consumed(),
                };
            }
        };

        let token_kind = match first_char {
            ';' => TokenKind::Semi,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '@' => TokenKind::At,
            '+' => TokenKind::Plus,
            '!' => TokenKind::Bang,
            ':' => match self.peek() {
                Some(':') => {
                    self.bump();
                    TokenKind::ColonColon
                }
                _ => TokenKind::Colon,
            },
            '-' => match self.peek() {
                Some('>') => {
                    self.bump();
                    TokenKind::RArrow
                }
                _ => TokenKind::Minus,
            },
            '*' => TokenKind::Star,
            '/' => match self.peek() {
                Some('/') => {
                    self.bump();
                    while self.bump() != Some('\n') {}
                    TokenKind::LineComment
                }
                _ => TokenKind::Slash,
            },
            '=' => match self.peek() {
                Some('=') => {
                    self.bump();
                    TokenKind::EqEq
                }
                _ => TokenKind::Eq,
            },
            '&' => match self.peek() {
                Some('&') => {
                    self.bump();
                    TokenKind::AndAnd
                }
                _ => TokenKind::And,
            },
            '|' => match self.peek() {
                Some('|') => {
                    self.bump();
                    TokenKind::OrOr
                }
                _ => TokenKind::Or,
            },
            '%' => TokenKind::Percent,
            c if is_whitespace(c) => return self.next_token(),
            '0'..='9' => {
                let mut is_float = false;
                while !self.is_eof()
                    && self
                        .peek()
                        .map(|c| c.is_ascii_digit() || c == '.' && !is_float)
                        .unwrap_or(false)
                {
                    if self.peek().unwrap() == '.' {
                        is_float = true;
                    }
                    self.bump();
                }
                TokenKind::Literal {
                    kind: if is_float {
                        LiteralKind::Float {
                            base: Base::Decimal,
                            empty_exponent: true,
                        }
                    } else {
                        LiteralKind::Int {
                            base: Base::Decimal,
                            empty_int: false,
                        }
                    },
                }
            }
            '"' => {
                while !self.is_eof() && self.peek().map(|c| c != '"').unwrap_or(true) {
                    self.bump();
                }

                self.bump();

                TokenKind::Literal {
                    kind: LiteralKind::Str { terminated: true },
                }
            }
            c if c.is_ascii() => {
                let mut s = String::from(c);
                while !self.is_eof()
                    && self
                        .peek()
                        .map(|c| c.is_ascii_alphabetic() || c.is_ascii_digit() || c == '_')
                        .unwrap_or(false)
                {
                    if let Some(c) = self.bump() {
                        s.push(c);
                    }
                }

                s.as_str()
                    .try_into()
                    .map(TokenKind::Keyword)
                    .map(|keyword| match keyword {
                        TokenKind::Keyword(Keyword::Mut) => {
                            if let Some(last_token) = self.last_token() {
                                if last_token.kind == TokenKind::Keyword(Keyword::Let) {
                                    keyword
                                } else {
                                    TokenKind::Ident
                                }
                            } else {
                                keyword
                            }
                        }
                        keyword => keyword,
                    })
                    .unwrap_or(TokenKind::Ident)
            }
            _ => TokenKind::Unknown,
        };

        let token = Token {
            start,
            end: self.consumed(),
            kind: token_kind,
        };

        self.set_last_token(token);

        token
    }
}
