use crate::{
    cursor::Cursor,
    token::{Base, LiteralKind, Token, TokenKind},
};

pub mod cursor;
pub mod stream;
pub mod token;

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> {
    let mut cursor = Cursor::new(&input);
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

impl<'a> Cursor<'a> {
    pub fn next_token(&mut self) -> Token {
        let start = self.consumed();
        let first_char = match self.next() {
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
            ':' => TokenKind::Colon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '+' => TokenKind::Plus,
            '!' => TokenKind::Bang,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => match self.peek() {
                Some('/') => {
                    self.next();
                    while self.next() != Some('\n') {}
                    TokenKind::LineComment
                }
                _ => TokenKind::Slash,
            },
            '=' => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::EqEq
                }
                _ => TokenKind::Eq,
            },
            '&' => match self.peek() {
                Some('&') => {
                    self.next();
                    TokenKind::AndAnd
                }
                _ => TokenKind::And,
            },
            '|' => match self.peek() {
                Some('|') => {
                    self.next();
                    TokenKind::OrOr
                }
                _ => TokenKind::Or,
            },
            '%' => TokenKind::Percent,
            c if is_whitespace(c) => return self.next_token(),
            '0'..='9' => {
                while !self.is_eof() && self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                    self.next();
                }
                TokenKind::Literal {
                    kind: LiteralKind::Int {
                        base: Base::Decimal,
                        empty_int: false,
                    },
                }
            }
            c if c.is_ascii() => {
                while !self.is_eof()
                    && self
                        .peek()
                        .map(|c| c.is_ascii_alphabetic() || c.is_ascii_digit())
                        .unwrap_or(false)
                {
                    self.next();
                }
                TokenKind::Ident
            }
            _ => TokenKind::Unknown,
        };

        Token {
            start,
            end: self.consumed(),
            kind: token_kind,
        }
    }
}
