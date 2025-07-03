use core::str::Chars;

use crate::token::Token;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    consumed: usize,
    last_token: Option<Token>,
    total_length: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            consumed: 0,
            last_token: None,
            total_length: input.len(),
        }
    }

    pub fn next(&mut self) -> Option<char> {
        self.consumed += 1;
        self.chars.next()
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn consumed(&self) -> usize {
        self.consumed
    }

    pub fn is_eof(&self) -> bool {
        self.consumed >= self.total_length
    }

    pub fn last_token(&self) -> Option<&Token> {
        self.last_token.as_ref()
    }

    pub(super) fn set_last_token(&mut self, last_token: Token) {
        self.last_token = Some(last_token);
    }
}
