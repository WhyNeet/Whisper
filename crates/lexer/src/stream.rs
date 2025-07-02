use crate::token::Token;
use core::ops::{Deref, DerefMut};
use std::iter::Peekable;

pub struct TokenStream<'a> {
    source: &'a str,
    iterator: Peekable<Box<dyn Iterator<Item = Token> + 'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(source: &'a str) -> Self {
        let iterator = crate::tokenize(source);
        let boxed: Box<dyn Iterator<Item = Token> + 'a> = Box::new(iterator);
        let peekable = boxed.peekable();

        Self {
            source,
            iterator: peekable,
        }
    }
}

impl<'a> Deref for TokenStream<'a> {
    type Target = Peekable<Box<dyn Iterator<Item = Token> + 'a>>;

    fn deref(&self) -> &Self::Target {
        &self.iterator
    }
}

impl<'a> DerefMut for TokenStream<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.iterator
    }
}

impl<'a> TokenStream<'a> {
    pub fn source(&self) -> &str {
        self.source
    }
}
