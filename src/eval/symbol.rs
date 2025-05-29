use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);


pub struct SymbolTable {
    resolve_table: HashMap<Symbol, Rc<str>>,
    string_stor: HashMap<Rc<str>, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            resolve_table: HashMap::new(),
            string_stor: HashMap::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(symbol) = self.string_stor.get(name) {
            *symbol
        } else {
            let name_cloned: Rc<str> = Rc::from(name);
            let symbol = Symbol(name_cloned.as_ptr() as usize);
            
            self.string_stor.insert(name_cloned.clone(), symbol);
            self.resolve_table.insert(symbol, name_cloned);

            symbol
        }
    }

    pub fn resolve(&self, symbol: &Symbol) -> Option<&str> {
        Some(self.resolve_table.get(symbol)?.as_ref())
    }
}

