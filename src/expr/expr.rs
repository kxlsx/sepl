use std::{fmt, collections::LinkedList, hash::Hash};

use super::{Symbol, Lit, Builtin, Procedure, Env, EnvTable, Error, Result};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr<'i> {
    Symbol(Symbol<'i>),
    Lit(Lit),
    Call(Box<Expr<'i>>, LinkedList<Expr<'i>>),
    Procedure(Procedure<'i>),
    Builtin(Builtin),
}

impl<'i> Expr<'i> {
    pub fn eval(self, env_table: &mut EnvTable<'i>, env: Env) -> Result<Self> {
        match self {
            Expr::Lit(lit) => 
                Ok(Expr::Lit(lit)),
            Expr::Builtin(builtin) =>
                Ok(Expr::Builtin(builtin)),
            Expr::Procedure(proc) =>
                Ok(Expr::Procedure(proc)),
            Expr::Symbol(symbol) => {
                if let Ok(expr) = env_table.resolve_symbol(symbol, env) {
                    expr.clone().eval(env_table, env)
                } else {
                    Ok(Expr::Symbol(symbol))
                }
            }
            Expr::Call(head, tail) => {
                let a = head.eval(env_table, env);
                match a? {
                    Expr::Procedure(proc) => proc.eval(env_table, env, tail),
                    Expr::Builtin(builtin) => builtin.eval(env_table, env, tail),
                    _ => Err(Error::NotCallable)
                }
            }
        }
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Lit(lit) => lit.fmt(f),
            Expr::Symbol(symbol) => symbol.fmt(f),
            Expr::Procedure(proc) => proc.fmt(f),
            Expr::Builtin(builtin) => builtin.fmt(f),
            Expr::Call(head, tail ) => {
                write!(f, "({}", head)?;
                for expr in tail {
                    write!(f, " {}", expr)?;
                }
                write!(f, ")")
            }
        }
    }
}
