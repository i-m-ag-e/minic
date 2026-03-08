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
