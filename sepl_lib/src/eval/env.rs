use std::collections::{HashMap, hash_map::Iter as HashMapIter};

use strum::IntoEnumIterator;

use crate::eval::{Builtin, Expr, Symbol, SymbolTable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Env(u64);

impl Env {
    pub const fn global() -> Self {
        EnvAllocator::env_global()
    }
}

#[derive(Debug)]
pub struct EnvTable {
    env_allocator: EnvAllocator,
    env_tree: HashMap<Env, EnvTreeNode>,
    symbol_definitions: HashMap<Env, HashMap<Symbol, Expr>>,
}

impl EnvTable {
    pub fn new() -> Self {
        EnvTable {
            env_allocator: EnvAllocator::new(),
            env_tree: HashMap::from([(Env::global(), EnvTreeNode::global())]),
            symbol_definitions: HashMap::new(),
        }
    }

    pub fn with_builtins(symbol_table: &mut SymbolTable) -> Self {
        let mut env_table = Self::new();

        for builtin in Builtin::iter() {
            let symbol = symbol_table.intern(builtin.as_ref());
            env_table.symbol_define_global(symbol, Expr::Builtin(builtin));
        }

        env_table
    }

    pub fn symbol_define_global(&mut self, symbol: Symbol, expr: Expr) -> Option<Expr> {
        self.symbol_define(symbol, self.env_global(), expr)
    }

    pub fn symbol_define(&mut self, symbol: Symbol, env: Env, expr: Expr) -> Option<Expr> {
        if let Expr::Procedure(procedure) = &expr {
            self.increment_capturing_count(procedure.captured_env());
        }

        self.symbol_definitions
            .entry(env)
            .or_default()
            .insert(symbol, expr)
    }

    pub fn symbol_definition(&self, symbol: Symbol, env: Env) -> Option<&Expr> {
        let definition_option = self
            .symbol_definitions
            .get(&env)
            .and_then(|symbol_defs| symbol_defs.get(&symbol));
        match (env, definition_option) {
            (env, result) if env == self.env_global() => result,
            (_, Some(expr)) => Some(expr),
            (env, None) => self.symbol_definition(symbol, self.get_captured_env(env)),
        }
    }

    pub fn symbols_global(&self) -> Option<HashMapIter<Symbol, Expr>> {
        self.symbol_definitions
        .get(&self.env_global())
        .map(|map| map.iter())
    }

    pub fn env_global(&self) -> Env {
        Env::global()
    }

    pub fn env_count(&self) -> usize {
        self.env_tree.len()
    }

    pub fn env_create(&mut self, captured_env: Env) -> Env {
        let env = self.env_allocator.env_alloc();
        self.insert_env(env, captured_env);
        env
    }

    pub fn env_try_destroy(&mut self, env: Env) -> bool {
        if env == Env::global() {
            return false;
        }
        if !self.env_tree.contains_key(&env) {
            return false;
        }
        if self.get_capturing_count(env) != 0 {
            return false;
        }

        let captured_env = self.get_captured_env(env);

        self.remove_node(env);
        self.decrement_capturing_count(captured_env);

        if let Some(symbol_defs) = self.symbol_definitions.remove(&env) {
            for (_, expr) in symbol_defs.iter() {
                if let Expr::Procedure(procedure) = expr {
                    self.decrement_capturing_count(procedure.captured_env())
                }
            }
        }

        self.env_allocator.env_free(env);

        true
    }

    fn insert_env(&mut self, env: Env, captured_env: Env) {
        self.insert_node(
            env,
            EnvTreeNode {
                captured_env,
                capturing_count: 0,
            },
        );
        self.increment_capturing_count(captured_env);
    }

    fn insert_node(&mut self, env: Env, node: EnvTreeNode) {
        match self.env_tree.insert(env, node) {
            None => (),
            Some(_) => panic!("Tried to insert existing Env."),
        }
    }

    fn remove_node(&mut self, env: Env) {
        self.env_tree
            .remove(&env)
            .expect("Tried to remove non-existent Env");
    }

    fn get_captured_env(&self, env: Env) -> Env {
        self.get_node(env).captured_env
    }

    fn get_capturing_count(&self, env: Env) -> usize {
        self.get_node(env).capturing_count
    }

    fn decrement_capturing_count(&mut self, env: Env) {
        let node = self.get_node_mut(env);
        node.capturing_count -= 1;

        if node.capturing_count == 0 {
            self.env_try_destroy(env);
        }
    }

    fn increment_capturing_count(&mut self, env: Env) {
        self.get_node_mut(env).capturing_count += 1
    }

    fn get_node(&self, env: Env) -> &EnvTreeNode {
        self.env_tree
            .get(&env)
            .expect("Tried to get non-existent Env.")
    }

    fn get_node_mut(&mut self, env: Env) -> &mut EnvTreeNode {
        self.env_tree
            .get_mut(&env)
            .expect("Tried to get mutable non-existent Env.")
    }
}

impl Default for EnvTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct EnvAllocator {
    last_id: u64,
    unused_ids: Vec<u64>,
}

impl EnvAllocator {
    pub fn new() -> Self {
        Self {
            last_id: 1,
            unused_ids: Vec::new(),
        }
    }

    pub const fn env_global() -> Env {
        Env(0)
    }

    pub fn env_alloc(&mut self) -> Env {
        if let Some(id) = self.unused_ids.pop() {
            Env(id)
        } else {
            let id = self.last_id;
            self.last_id += 1;
            Env(id)
        }
    }

    pub fn env_free(&mut self, env: Env) {
        if env.0 == self.last_id - 1 {
            self.last_id -= 1;
        } else {
            self.unused_ids.push(env.0);
        }
    }
}

#[derive(Debug)]
struct EnvTreeNode {
    captured_env: Env,
    capturing_count: usize,
}

impl EnvTreeNode {
    fn global() -> Self {
        Self {
            captured_env: Env::global(),
            capturing_count: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    //TODO: EnvTable tests
}
