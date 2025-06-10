use core::str;
use std::{collections::HashMap, slice::from_raw_parts};

/// Type represeting any `symbol` value.
/// Every [`Symbol`] represents a string of
/// characters [interned](SymbolTable::intern)
/// in the [`SymbolTable`] and can be converted
/// into a [`&str`](prim@str) using [`SymbolTable::resolve`].
///
/// <div class="warning">
/// Warning!
///
/// Trying to [`resolve`](SymbolTable::resolve) a [`Symbol`]
/// in a [`SymbolTable`] it has not originated from
/// is an error and can cause **Undefined Behaviour**.
/// </div>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(*const u8, usize);

/// Struct used for [`String` interning](https://en.wikipedia.org/wiki/String_interning).
/// The [`intern`](SymbolTable::intern) and [`resolve`](SymbolTable::resolve)
/// methods are used to convert to-and-from a [`String`]'s [`Symbol`]ic
/// representation.
///
/// <div class="warning">
/// Warning!
///
/// Trying to [`resolve`](SymbolTable::resolve) a [`Symbol`]
/// in a [`SymbolTable`] it has not originated from
/// is an error and can cause **Undefined Behaviour**.
/// </div>
///
/// # Example
/// ```
/// use sepl_lib::eval::SymbolTable;
///
/// let mut symbol_table = SymbolTable::new();
///
/// let symbol1 = symbol_table.intern("skrzat");
/// let symbol2 = symbol_table.intern("środa");
///
/// assert_eq!(symbol_table.resolve(symbol1), "skrzat");
/// assert_eq!(symbol_table.resolve(symbol2), "środa");
/// ```
#[derive(Debug)]
pub struct SymbolTable {
    string_stor: HashMap<Box<str>, Symbol>,
}

impl SymbolTable {
    /// Creates an empty [`SymbolTable`].
    pub fn new() -> Self {
        Self {
            string_stor: HashMap::new(),
        }
    }

    /// Interns the `name` in the [`SymbolTable`].
    /// The name is cloned and stored in the table
    /// if it's not already stored. The method
    /// returns a [`Symbol`] representing
    /// the passed `name`.
    ///
    /// # Example
    /// ```
    /// use sepl_lib::eval::SymbolTable;
    ///
    /// let mut symbol_table = SymbolTable::new();
    ///
    /// let symbol1 = symbol_table.intern("kamil");
    /// let symbol2 = symbol_table.intern("ślimak");
    /// let symbol3 = symbol_table.intern("ślimak");
    ///
    /// assert_ne!(symbol1, symbol2);
    /// assert_eq!(symbol2, symbol3);
    /// ```
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

    /// Return the [`&str`](prim@str) representation
    /// of the passed [`Symbol`].
    ///
    /// <div class="warning">
    /// Warning!
    ///
    /// Trying to [`resolve`](SymbolTable::resolve) a [`Symbol`]
    /// in a [`SymbolTable`] it has not originated from
    /// is an error and can cause **Undefined Behaviour**.
    /// </div>
    ///
    /// # Example
    /// ```
    /// use sepl_lib::eval::SymbolTable;
    ///
    /// let mut symbol_table = SymbolTable::new();
    ///
    /// let symbol1 = symbol_table.intern("foo");
    /// let symbol2 = symbol_table.intern("bar");
    ///
    /// assert_eq!(symbol_table.resolve(symbol1), "foo");
    /// assert_eq!(symbol_table.resolve(symbol2), "bar");
    /// ```
    pub fn resolve(&self, symbol: Symbol) -> &str {
        let Symbol(ptr, len) = symbol;

        // This works, because SymbolTable stores Box<str>, which
        // are all unique heap allocations.
        unsafe {
            let resolved = str::from_utf8_unchecked(from_raw_parts(ptr, len));
            debug_assert!(
                self.string_stor.contains_key(resolved),
                "{:?} does not exist in this SymbolTable!",
                symbol
            );

            resolved
        }
    }

    /// Return the number of [`String`]s interned
    /// in the [`SymbolTable`].
    pub fn len(&self) -> usize {
        self.string_stor.len()
    }

    /// Return whether the [`SymbolTable`]
    /// is empty.
    pub fn is_empty(&self) -> bool {
        self.string_stor.len() == 0
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol_interning() {
        let mut symbol_table = SymbolTable::new();

        for _ in 1..30 {
            symbol_table.intern("goblin");
        }
        for _ in 1..30 {
            symbol_table.intern("orzechy");
        }

        assert_eq!(symbol_table.len(), 2);
    }

    #[test]
    fn symbol_equality() {
        let mut symbol_table = SymbolTable::new();

        let franek1 = symbol_table.intern("franek");
        let franek2 = symbol_table.intern("franek");
        let puzon = symbol_table.intern("puzon");

        assert_eq!(franek1, franek2);
        assert_ne!(franek1, puzon);
    }

    #[test]
    fn symbol_resolve() {
        let mut symbol_table = SymbolTable::new();

        let francis1 = symbol_table.intern("francis");
        let francis2 = symbol_table.intern("francis");
        let trombone = symbol_table.intern("trombone");

        assert_eq!(symbol_table.resolve(francis1), "francis");
        assert_eq!(symbol_table.resolve(francis2), "francis");
        assert_eq!(symbol_table.resolve(trombone), "trombone");
    }
}
