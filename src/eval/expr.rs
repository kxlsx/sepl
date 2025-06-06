use std::hash::Hash;

use super::{Builtin, Env, Error, EnvTable, Lit, Procedure, Symbol};

macro_rules! expr_type_str {
    (Symbol) => {
        "symbol"
    };
    (Lit::Bool) => {
        "bool"
    };
    (Lit::Float) => {
        "float"
    };
    (Lit::Nil) => {
        "nil"
    };
    (Procedure) => {
        "procedure"
    };
    (Builtin) => {
        "procedure"
    };
    (Call) => {
        "procedure call"
    };
}
pub(super) use expr_type_str;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Symbol(Symbol),
    Lit(Lit),
    Call(Box<Expr>, Vec<Expr>),
    Procedure(Procedure),
    Builtin(Builtin),
}

impl Expr {
    pub fn eval(self, env_table: &mut EnvTable, env: Env) -> Result<Self, Error> {
        match self {
            Expr::Lit(lit) => Ok(Expr::Lit(lit)),
            Expr::Builtin(builtin) => Ok(Expr::Builtin(builtin)),
            Expr::Procedure(proc) => Ok(Expr::Procedure(proc)),
            Expr::Symbol(symbol) => match env_table.symbol_definition(symbol, env) {
                Some(expr) => expr.clone().eval(env_table, env),
                None => Ok(Expr::Symbol(symbol)),
            },
            Expr::Call(head, tail) => match head.eval(env_table, env)? {
                Expr::Procedure(proc) => proc.eval(env_table, env, tail),
                Expr::Builtin(builtin) => builtin.eval(env_table, env, tail),
                uncallable_expr => Err(Error::NotCallable {
                    expr: uncallable_expr,
                }),
            },
        }
    }

    pub const fn as_type_str(&self) -> &'static str {
        match self {
            Expr::Lit(Lit::Bool(_)) => expr_type_str!(Lit::Bool),
            Expr::Lit(Lit::Float(_)) => expr_type_str!(Lit::Float),
            Expr::Lit(Lit::Nil) => expr_type_str!(Lit::Nil),
            Expr::Symbol(_) => expr_type_str!(Symbol),
            Expr::Procedure(_) => expr_type_str!(Procedure),
            Expr::Builtin(_) => expr_type_str!(Builtin),
            Expr::Call(..) => expr_type_str!(Call),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::ParseFrom;

    use super::super::SymbolTable;
    use super::*;

    macro_rules! assert_evals_from_str {
        (with $symbol_table:ident, $env_table:ident: $($string:literal => $expected:pat),+, ) => {
            $({
            let tmp = Expr::parse_from(
                $string,
                &mut $symbol_table
            ).expect("Parse error!");
            
            let tmp_eval = tmp.eval(&mut $env_table, Env::global());
            if let $expected = tmp_eval {} else {panic!("Did not expect {:?}", tmp_eval)}
            })+
        };
    }

    #[test]
    fn eval_define() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "(define pi 3.1415)" => Ok(Expr::Lit(Lit::Nil)),
            "pi" => Ok(Expr::Lit(Lit::Float(3.1415))),
        );

        Ok(())
    }

    #[test]
    fn eval_define_recursive() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "(define x x)" => Ok(Expr::Lit(Lit::Nil)),
            "x" => Ok(Expr::Symbol(_)),
            "(define x y)" => Ok(Expr::Lit(Lit::Nil)),
            "(define y z)" => Ok(Expr::Lit(Lit::Nil)),
            "(define z x)" => Ok(Expr::Lit(Lit::Nil)),
            "x" => Ok(Expr::Symbol(_)),
            "y" => Ok(Expr::Symbol(_)),
            "z" => Ok(Expr::Symbol(_)),
        );

        Ok(())
    }

    #[test]
    fn eval_define_scope() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "(define x 2.0)" => Ok(Expr::Lit(Lit::Nil)),
            "(define y 4096.0)" => Ok(Expr::Lit(Lit::Nil)),
            "((lambda x (do (define y 1.) (+ x y))) 0.5)" 
                => Err(Error::IncorrectArgType { 
                    builtin: Builtin::Lambda,
                    expected: expr_type_str!(Symbol),
                    found: expr_type_str!(Lit::Float),
                }),
            "((lambda (quote x) (do (define y 1.) (+ x y))) 0.5)" 
                => Err(Error::IncorrectArgType { 
                    builtin: Builtin::Define,
                    expected: expr_type_str!(Symbol),
                    found: expr_type_str!(Lit::Float),
                }),
            "((lambda (quote x) (do (define (quote y) 1.) (+ x y))) 0.5)" 
                => Ok(Expr::Lit(Lit::Float(1.5))),
            "x" => Ok(Expr::Lit(Lit::Float(2.))),
            "y" => Ok(Expr::Lit(Lit::Float(4096.))),
        );

        Ok(())
    }

    #[test]
    fn eval_factorial() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "(define = (lambda a b (if (<= a b) (<= b a) false)))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(define fact (lambda n (if (= n 0.0) 1. (* n (fact (- n 1.))))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(fact 10.)"
                => Ok(Expr::Lit(Lit::Float(3628800.0))),
        );

        Ok(())
    }

    #[test]
    fn eval_fib() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "(define = (lambda a b (if (<= a b) (<= b a) false)))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(define fib (lambda a b n (if (= n 0.0) a (fib b (+ a b) (- n 1.0)))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(define fib_bad (lambda n (if (= n 0.0) 0.0 (if (= n 1.0) 1.0 (+ (fib_bad (- n 1.0)) (fib_bad (- n 2.0)))))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(fib 0. 1. 10.)"
                => Ok(Expr::Lit(Lit::Float(55.))),
            "(fib_bad 10.)"
                => Ok(Expr::Lit(Lit::Float(55.))),
        );

        Ok(())
    }

    #[test]
    fn eval_curried() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "(define f (lambda a (lambda b (lambda c (+ a (+ b c))))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(((f 2.) 2.) 2.)"
                => Ok(Expr::Lit(Lit::Float(6.))),
        );

        Ok(())
    }

    #[test]
    fn eval_cons_as_lambda() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "(define cons (lambda x y (lambda m (m x y))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(define car (lambda z (z (lambda p q p))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(define cdr (lambda z (z (lambda p q q))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(define list (cons 1.0 (cons 2. (cons 3. nil))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(define sum (lambda xs (if (cdr xs) (+ (car xs) (sum (cdr xs))) (car xs))))"
                => Ok(Expr::Lit(Lit::Nil)),
            "(sum list)"
                => Ok(Expr::Lit(Lit::Float(6.))),
        );

        Ok(())
    }

    #[test]
    fn eval_lits() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "42."   => Ok(Expr::Lit(Lit::Float(42.))),
            "true"  => Ok(Expr::Lit(Lit::Bool(true))),
            "false" => Ok(Expr::Lit(Lit::Bool(false))),
            "nil"   => Ok(Expr::Lit(Lit::Nil)),
        );

        Ok(())
    }

    #[test]
    fn eval_not_callable() -> Result<(), Error> {
        let mut symbol_table = SymbolTable::new();
        let mut env_table = EnvTable::with_builtins(&mut symbol_table);

        assert_evals_from_str!(
            with symbol_table, env_table:
            "((+ 2. 3.) e)"  => Err(Error::NotCallable { .. }),
        );

        Ok(())
    }
}
