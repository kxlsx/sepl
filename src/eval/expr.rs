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

#[cfg(test)]
mod tests {
    use crate::parse::ParseFrom;

    use super::super::SymbolTable;
    use super::*;

    // TODO: more expr tests

    macro_rules! assert_evals_from_str {
        (with $symbol_table:ident, $eval_table:ident: $($string:literal => $expected:expr),+, ) => {
            $({
            let tmp = Expr::parse_from(
                $string,
                &mut $symbol_table
            ).expect("Parse error!");

            assert_eq!(tmp.eval(&mut $eval_table, Env::global())?, $expected);
            })+
        };
    }

    #[test]
    fn eval_define() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut eval_table = EvalTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, eval_table:
            "(define pi 3.1415)" => Expr::Lit(Lit::Nil),
            "pi" => Expr::Lit(Lit::Float(3.1415)),
        );

        Ok(())
    }

    #[test]
    fn eval_factorial() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut eval_table = EvalTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, eval_table:
            "(define = (lambda a b (if (<= a b) (<= b a) false)))"
                => Expr::Lit(Lit::Nil),
            "(define fact (lambda n (if (= n 0.0) 1. (* n (fact (- n 1.))))))" 
                => Expr::Lit(Lit::Nil),
            "(fact 10.)" 
                => Expr::Lit(Lit::Float(3628800.0)),
        );

        Ok(())
    }

    #[test]
    fn eval_fib() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut eval_table = EvalTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, eval_table:
            "(define = (lambda a b (if (<= a b) (<= b a) false)))" 
                => Expr::Lit(Lit::Nil),
            "(define fib (lambda a b n (if (= n 0.0) a (fib b (+ a b) (- n 1.0)))))" 
                => Expr::Lit(Lit::Nil),
            "(define fib_bad (lambda n (if (= n 0.0) 0.0 (if (= n 1.0) 1.0 (+ (fib_bad (- n 1.0)) (fib_bad (- n 2.0)))))))" 
                => Expr::Lit(Lit::Nil),
            "(fib 0. 1. 10.)" 
                => Expr::Lit(Lit::Float(55.)),
            "(fib_bad 10.)" 
                => Expr::Lit(Lit::Float(55.)),
        );

        Ok(())
    }

    #[test]
    fn eval_curried() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut eval_table = EvalTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, eval_table:
            "(define f (lambda a (lambda b (lambda c (+ a (+ b c))))))" 
                => Expr::Lit(Lit::Nil),
            "(((f 2.) 2.) 2.)" 
                => Expr::Lit(Lit::Float(6.)),
        );

        Ok(())
    }

    #[test]
    fn eval_cons_as_lambda() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut eval_table = EvalTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, eval_table:
            "(define cons (lambda x y (lambda m (m x y))))" 
                => Expr::Lit(Lit::Nil),
            "(define car (lambda z (z (lambda p q p))))" 
                => Expr::Lit(Lit::Nil),
            "(define cdr (lambda z (z (lambda p q q))))" 
                => Expr::Lit(Lit::Nil),
            "(define list (cons 1.0 (cons 2. (cons 3. nil))))" 
                => Expr::Lit(Lit::Nil),
            "(define sum (lambda xs (if (cdr xs) (+ (car xs) (sum (cdr xs))) (car xs))))" 
                => Expr::Lit(Lit::Nil),
            "(sum list)" 
                => Expr::Lit(Lit::Float(6.)),
        );

        Ok(())
    }

    #[test]
    fn eval_lits() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut eval_table = EvalTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, eval_table:
            "42."   => Expr::Lit(Lit::Float(42.)),
            "true"  => Expr::Lit(Lit::Bool(true)),
            "false" => Expr::Lit(Lit::Bool(false)),
            "nil"   => Expr::Lit(Lit::Nil),
        );

        Ok(())
    }
}