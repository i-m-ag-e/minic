use std::collections::HashMap;

use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Symbol(pub usize);

#[derive(Debug)]
pub struct SymbolTable {
    symbols: Vec<String>,
    map: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&symbol) = self.map.get(name) {
            symbol
        } else {
            let symbol = Symbol(self.symbols.len());
            self.symbols.push(name.to_string());
            self.map.insert(name.to_string(), symbol);
            symbol
        }
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.symbols.get(symbol.0).map(|s| s.as_str())
    }
}
