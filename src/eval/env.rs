use std::collections::HashMap;

use super::{Error, Result};
use crate::eval::{Expr, Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Env(u32);

impl Env {
    pub fn global() -> Self {
        Env(0)
    }
}

// TODO: better representation than hash table (removing envs)
pub struct EnvTable {
    pub symbol_table: HashMap<(Symbol, Env), Expr>,
    env_parent_table: HashMap<Env, Env>,
    env_global: Env,
    env_next: Env,
}

impl EnvTable {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
            env_parent_table: HashMap::from([(Env::global(), Env::global())]),
            env_global: Env::global(),
            env_next: Env(1),
        }
    }

    pub fn define_symbol(&mut self, symbol: Symbol, env: Env, expr: Expr) {
        self.symbol_table.insert((symbol, env), expr);
    }

    pub fn undefine_symbol(&mut self, symbol: Symbol, env: Env) {
        self.symbol_table.remove(&(symbol, env));
    }

    pub fn define_global_symbol(&mut self, symbol: Symbol, expr: Expr) {
        self.define_symbol(symbol, Env::global(), expr);
    }

    pub fn resolve_symbol(&self, symbol: Symbol, env: Env) -> Result<&Expr> {
        if let Some(expr) = self.symbol_table.get(&(symbol, env)) {
            Ok(expr)
        } else if env != self.env_global {
            Ok(self.resolve_symbol(symbol, self.parent_env(env))?)
        } else {
            Err(Error::UndefinedSymbol)
        }
    }

    pub fn create_env(&mut self, parent_env: Env) -> Env {
        let env = self.new_env();

        self.env_parent_table.insert(env, parent_env);

        env
    }

    fn parent_env(&self, env: Env) -> Env {
        *self.env_parent_table.get(&env).expect("TODO:")
    }

    fn new_env(&mut self) -> Env {
        let Env(e) = self.env_next;

        self.env_next = Env(e + 1);
        Env(e)
    }
}
