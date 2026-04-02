use std::collections::HashMap;

use crate::{type_checker::r#type::Type, with_token::WithToken};

#[derive(Debug)]
pub struct TypedSymbolTableEntry {
    pub ty: Type,
    pub token: WithToken<()>,
}

pub type TypedSymbolTable = HashMap<String, TypedSymbolTableEntry>;
