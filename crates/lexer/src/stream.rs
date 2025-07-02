use crate::token::Token;
use core::ops::{Deref, DerefMut};
use std::iter::Peekable;

pub struct TokenStream<'a, S: Iterator<Item = Token>> {
    source: &'a str,
    iterator: Peekable<S>,
}

impl<'a, S: Iterator<Item = Token>> Deref for TokenStream<'a, S> {
    type Target = Peekable<S>;

    fn deref(&self) -> &Self::Target {
        &self.iterator
    }
}

impl<'a, S: Iterator<Item = Token>> DerefMut for TokenStream<'a, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.iterator
    }
}

impl<'a, S: Iterator<Item = Token>> TokenStream<'a, S> {
    pub fn source(&self) -> &str {
        self.source
    }
}
