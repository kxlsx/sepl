use std::{collections::LinkedList, hash::Hash};

use super::{Builtin, Env, Error, EvalTable, Lit, Procedure, Symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Symbol(Symbol),
    Lit(Lit),
    Call(Box<Expr>, LinkedList<Expr>),
    Procedure(Procedure),
    Builtin(Builtin),
}

impl Expr {
    pub fn eval(self, eval_table: &mut EvalTable, env: Env) -> Result<Self, Error> {
        match self {
            Expr::Lit(lit) => Ok(Expr::Lit(lit)),
            Expr::Builtin(builtin) => Ok(Expr::Builtin(builtin)),
            Expr::Procedure(proc) => Ok(Expr::Procedure(proc)),
            Expr::Symbol(symbol) => match eval_table.symbol_definition(symbol, env) {
                Some(expr) => expr.clone().eval(eval_table, env),
                None => Ok(Expr::Symbol(symbol)),
            },
            Expr::Call(head, tail) => match head.eval(eval_table, env)? {
                Expr::Procedure(proc) => proc.eval(eval_table, env, tail),
                Expr::Builtin(builtin) => builtin.eval(eval_table, env, tail),
                _ => Err(Error::NotCallable),
            },
        }
    }
}
