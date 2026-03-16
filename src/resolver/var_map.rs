use std::collections::HashMap;

use crate::{symbol::Symbol, with_token::WithToken};

type VarMap = HashMap<String, WithToken<Symbol>>;

#[derive(Debug)]
pub struct ScopedVarMap {
    pub scope_stack: Vec<VarMap>,
    symbols: Vec<String>,
}

impl ScopedVarMap {
    pub fn new() -> Self {
        ScopedVarMap {
            scope_stack: vec![VarMap::new()],
            symbols: Vec::new(),
        }
    }

    pub fn add_scope(&mut self) {
        self.scope_stack.push(VarMap::new());
    }

    pub fn unwind(&mut self) -> Option<VarMap> {
        self.scope_stack.pop()
    }

    pub fn current_scope(&self) -> &VarMap {
        self.scope_stack
            .last()
            .expect("there must be atleast one scope at ll times in the ScopedVarMap")
    }

    pub fn current_scope_mut(&mut self) -> &mut VarMap {
        self.scope_stack
            .last_mut()
            .expect("there must be atleast one scope at ll times in the ScopedVarMap")
    }

    pub fn nth_innermost_scope(&self, n: usize) -> Option<&VarMap> {
        self.scope_stack.get(self.scope_stack.len() - n - 1)
    }

    pub fn insert(&mut self, name: String, resolved_name: String, token: WithToken<()>) -> Symbol {
        let symbol = Symbol(self.symbols.len());
        self.symbols.push(resolved_name);
        self.current_scope_mut()
            .insert(name, token.with_value(symbol));
        symbol
    }

    pub fn lookup(&self, name: &str) -> Option<WithToken<Symbol>> {
        (0..self.scope_stack.len())
            .find_map(|n| {
                self.nth_innermost_scope(n)
                    .and_then(|scope| scope.get(name))
            })
            .copied()
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.symbols.get(symbol.0).map(|s| s.as_str())
    }

    pub fn resolve_assert(&self, symbol: Symbol) -> &str {
        self.resolve(symbol)
            .expect("all variables must be resolved after the validate stage")
    }
}
