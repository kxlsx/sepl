use std::{collections::LinkedList, hash::Hash};

use super::{Builtin, Env, EnvTable, Error, Lit, Procedure, Result, Symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Symbol(Symbol),
    Lit(Lit),
    Call(Box<Expr>, LinkedList<Expr>),
    Procedure(Procedure),
    Builtin(Builtin),
}

impl Expr {
    pub fn eval(self, env_table: &mut EnvTable, env: Env) -> Result<Self> {
        match self {
            Expr::Lit(lit) => Ok(Expr::Lit(lit)),
            Expr::Builtin(builtin) => Ok(Expr::Builtin(builtin)),
            Expr::Procedure(proc) => Ok(Expr::Procedure(proc)),
            Expr::Symbol(symbol) => {
                if let Ok(expr) = env_table.resolve_symbol(symbol, env) {

                    expr.clone().eval(env_table, env)
                } else {
                    Ok(Expr::Symbol(symbol))
                }
            }
            Expr::Call(head, tail) => {
                match head.eval(env_table, env)? {
                    Expr::Procedure(proc) => proc.eval(env_table, env, tail),
                    Expr::Builtin(builtin) => builtin.eval(env_table, env, tail),
                    _ => Err(Error::NotCallable),
                }
            }
        }
    }
}
