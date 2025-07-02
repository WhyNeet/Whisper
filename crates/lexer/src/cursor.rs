use core::str::Chars;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    consumed: usize,
    total_length: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            consumed: 0,
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
}
