use std::collections::HashMap;

use crate::{symbol::Symbol, with_token::WithToken};

#[derive(Debug)]
pub struct VarMap {
    pub map: HashMap<String, WithToken<Symbol>>,
    symbols: Vec<String>,
}

impl VarMap {
    pub fn new() -> Self {
        VarMap {
            map: HashMap::new(),
            symbols: Vec::new(),
        }
    }

    pub fn insert(&mut self, name: String, resolved_name: String, token: WithToken<()>) -> Symbol {
        let symbol = Symbol(self.symbols.len());
        self.symbols.push(resolved_name);
        self.map.insert(name, token.with_value(symbol));
        symbol
    }

    pub fn get(&self, name: &str) -> Option<WithToken<Symbol>> {
        self.map.get(name).copied()
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.symbols.get(symbol.0).map(|s| s.as_str())
    }

    pub fn resolve_assert(&self, symbol: Symbol) -> &str {
        self.resolve(symbol)
            .expect("all variables must be resolved after the validate stage")
    }
}
