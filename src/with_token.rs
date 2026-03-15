use std::ops::Deref;

use crate::lexer::token::{Token, TokenID};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct WithToken<T> {
    pub item: T,
    pub token_id: TokenID,
}

impl<T> WithToken<T> {
    pub fn new(item: T, token_id: TokenID) -> Self {
        Self {
            item: item,
            token_id,
        }
    }

    pub fn get_token<'a>(&self, tokens: &'a [Token]) -> &'a Token {
        &tokens[self.token_id]
    }

    pub fn into_boxed(self) -> WithToken<Box<T>> {
        WithToken::new(Box::new(self.item), self.token_id)
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> WithToken<U> {
        WithToken::new(f(self.item), self.token_id)
    }

    pub fn map_transpose_result<U, E, F: FnOnce(T) -> Result<U, E>>(
        self,
        f: F,
    ) -> Result<WithToken<U>, E> {
        self.map(f).transpose()
    }

    pub fn with_value<U>(&self, new_item: U) -> WithToken<U> {
        WithToken::new(new_item, self.token_id)
    }

    pub fn unwrap(self) -> T {
        self.item
    }
}

impl<T> Deref for WithToken<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T, E> WithToken<Result<T, E>> {
    pub fn transpose(self) -> Result<WithToken<T>, E> {
        self.item.map(|v| WithToken::new(v, self.token_id))
    }
}
