use core::str;
use std::{collections::HashMap, slice::from_raw_parts};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(*const u8, usize);

#[derive(Debug)]
pub struct SymbolTable {
    string_stor: HashMap<Box<str>, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            string_stor: HashMap::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(symbol) = self.string_stor.get(name) {
            *symbol
        } else {
            let name_cloned: Box<str> = Box::from(name);
            let symbol = Symbol(name_cloned.as_ptr(), name_cloned.len());

            self.string_stor.insert(name_cloned, symbol);

            symbol
        }
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        let Symbol(ptr, len) = symbol;

        unsafe { str::from_utf8_unchecked(from_raw_parts(ptr, len)) }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    //TODO: symboltable tests
}