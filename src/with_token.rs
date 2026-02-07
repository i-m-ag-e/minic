use std::ops::Deref;

use crate::lexer::token::{Token, TokenID};

#[derive(Debug, Clone, PartialEq)]
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
}

impl<T> Deref for WithToken<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}
