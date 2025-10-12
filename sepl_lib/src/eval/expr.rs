use std::{collections::LinkedList, hash::Hash};

use super::{Builtin, Env, EnvTable, Error, Lit, Procedure, Symbol};

/// Macro used to return
/// a static string representing
/// an expression's type.
///
/// Accepts the macro variant name
/// as input (without the path).
macro_rules! expr_type_str {
    (Symbol) => {
        "symbol"
    };
    (Lit::Float) => {
        "float"
    };
    (Procedure) => {
        "procedure"
    };
    (Builtin) => {
        "procedure"
    };
    (List) => {
        "list"
    };
}
pub(super) use expr_type_str;

/// Type representing an expression in the
/// `sepl` language.
///
/// This is **the** basic building block of
/// the language, serving both as AST[^ast] and IR[^ir].
/// The enum variants can also be thought of
/// as types.
///
/// [`Expr`] can be constructed
/// using a [`Parser`](crate::parse::Parser) and
/// evaluated (i.e. recursively reduced) using the
/// [`eval`](Expr::eval) and [`eval_global`](Expr::eval_global)
/// functions.
///
/// Expressions are always evaluated in a specified
/// environment (scope), which they can possibly modify.
/// The passed environment and the global state are
/// represented by the [`Env`] and [`EnvTable`] structs.
///
/// # Example
/// ```
/// use sepl_lib::{
///     eval::{Expr, EnvTable, Lit},
///     parse::ParseFrom,
/// };
///
/// let mut env_table = EnvTable::with_builtins();
/// 
/// let expr = Expr::parse_from(
///     "((lambda (x) (* x x)) 16.0)",
///     &mut env_table
///     ).unwrap();
///
/// assert!(matches!(
///     expr.eval_global(&mut env_table),
///     Ok(Expr::Lit(Lit::Float(256.)))
/// ))
/// ```
///
/// [^ast]: [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
/// [^ir]: [Intermediate Representation](https://en.wikipedia.org/wiki/Intermediate_representation)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Symbol(Symbol),
    Lit(Lit),
    List(LinkedList<Expr>),
    Procedure(Procedure),
    Builtin(Builtin),
}

impl Expr {
    /// [`eval`](Expr::eval), but called with [`EnvTable::env_global`](crate::eval::EnvTable::env_global).
    pub fn eval_global(self, env_table: &mut EnvTable) -> Result<Self, Error> {
        self.eval(env_table, env_table.env_global())
    }

    /// Evaluate the [`Expr`] using the passed
    /// environment state ([`EnvTable`]) in the passed
    /// environment ([`Env`]). If you don't need
    /// to specify the environment, use [`eval_global`](Expr::eval_global).
    ///
    /// Evaluation rules are described in here: [`sepl_lib`](crate).
    ///
    /// # Errors
    /// [`Error::IncorrectArgCount`] and [`Error::IncorrectArgType`]
    /// are returned when evaluating a [`Procedure`] or a [`Builtin`]
    /// with incorrect arguments.
    ///
    /// # Examples
    /// ```
    /// use sepl_lib::{
    ///     eval::{Expr, EnvTable, Lit, Error},
    ///     parse::ParseFrom,
    /// };
    ///
    /// let mut env_table = EnvTable::with_builtins();
    ///
    /// let expr = Expr::parse_from(
    ///     "(+ 2. (* 3. 4.))",
    ///     &mut env_table
    /// ).unwrap();
    ///
    /// assert!(matches!(
    ///     expr.eval_global(&mut env_table),
    ///     Ok(Expr::Lit(Lit::Float(14.)))
    /// ));
    /// ```
    pub fn eval(self, env_table: &mut EnvTable, env: Env) -> Result<Self, Error> {
        match self {
            Expr::Lit(lit) => Ok(Expr::Lit(lit)),
            Expr::Builtin(builtin) => Ok(Expr::Builtin(builtin)),
            Expr::Procedure(proc) => Ok(Expr::Procedure(proc)),
            Expr::Symbol(symbol) => match env_table.symbol_definition(symbol, env) {
                Some(expr) => Ok(expr.clone()),
                None => Ok(Expr::Symbol(symbol)),
            },
            // TODO: Eval args before calling procedure
            Expr::List(mut list) => match list.pop_front().map(|e| e.eval(env_table, env)) {
                Some(Ok(Expr::Procedure(proc))) => proc.eval(env_table, env, list),
                Some(Ok(Expr::Builtin(builtin))) => builtin.eval(env_table, env, list),
                Some(Ok(other_expr)) => {
                    let mut tail = list
                        .into_iter()
                        .map(|e| e.eval(env_table, env))
                        .collect::<Result<LinkedList<Expr>, Error>>()?;
                    tail.push_front(other_expr);
                    Ok(Expr::List(tail))
                }
                Some(Err(error)) => Err(error),
                None => Ok(Expr::List(list)),
            },
        }
    }

    /// Return a [&'static str](str) representing
    /// the expressions type.
    ///
    /// # Example
    /// ```
    /// use sepl_lib::eval::{Expr, Lit};
    ///
    /// assert_eq!(
    ///     Expr::Lit(Lit::Float(10.)).as_type_str(),
    ///     "float"
    /// );
    /// ```
    pub const fn as_type_str(&self) -> &'static str {
        match self {
            Expr::Lit(Lit::Float(_)) => expr_type_str!(Lit::Float),
            Expr::Symbol(_) => expr_type_str!(Symbol),
            Expr::Procedure(_) => expr_type_str!(Procedure),
            Expr::Builtin(_) => expr_type_str!(Builtin),
            Expr::List(..) => expr_type_str!(List),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::ParseFrom;

    use super::*;

    macro_rules! assert_evals_from_str {
        (with $env_table:ident: $($string:literal => $expected:pat),+, ) => {
            $({
            let tmp = Expr::parse_from(
                $string,
                &mut $env_table
            ).expect("Parse error!");

            let tmp_eval = tmp.eval(&mut $env_table, Env::global());
            if let $expected = tmp_eval {} else {panic!("Did not expect {:?}", tmp_eval)}
            })+
        };
    }

    #[test]
    fn eval_define() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "(define pi 3.1415)" => Ok(_),
            "pi" => Ok(Expr::Lit(Lit::Float(3.1415))),
        );

        Ok(())
    }

    #[test]
    fn eval_define_recursive() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "(define x x)" => Ok(_),
            "x" => Ok(Expr::Symbol(_)),
            "(define x y)" => Ok(_),
            "(define y z)" => Ok(_),
            "(define z x)" => Ok(_),
            "x" => Ok(Expr::Symbol(_)),
            "y" => Ok(Expr::Symbol(_)),
            "z" => Ok(Expr::Symbol(_)),
        );

        Ok(())
    }

    #[test]
    fn eval_lambda_shadowing() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "((lambda (x) x) x)" => Ok(Expr::Symbol(_)),
        );

        Ok(())
    }

    #[test]
    fn eval_define_scope() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "(define x 2.0)" => Ok(_),
            "(define y 4096.0)" => Ok(_),
            "((lambda (x) (do (define y 1.) (+ x y))) 0.5)"
                => Err(Error::IncorrectArgType {
                    builtin: Builtin::Lambda,
                    expected: expr_type_str!(Symbol),
                    found: expr_type_str!(Lit::Float),
                }),
            "((lambda ((quote x)) (do (define y 1.) (+ x y))) 0.5)"
                => Err(Error::IncorrectArgType {
                    builtin: Builtin::Define,
                    expected: expr_type_str!(Symbol),
                    found: expr_type_str!(Lit::Float),
                }),
            "((lambda ((quote x)) (do (define (quote y) 1.) (+ x y))) 0.5)"
                => Ok(Expr::Lit(Lit::Float(1.5))),
            "x" => Ok(Expr::Lit(Lit::Float(2.))),
            "y" => Ok(Expr::Lit(Lit::Float(4096.))),
        );

        Ok(())
    }

    #[test]
    fn eval_factorial() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "(define = (lambda (a b) (if (<= a b) (<= b a) false)))"
                => Ok(_),
            "(define fact (lambda (n) (if (= n 0.0) 1. (* n (fact (- n 1.))))))"
                => Ok(_),
            "(fact 10.)"
                => Ok(Expr::Lit(Lit::Float(3628800.0))),
        );

        Ok(())
    }

    #[test]
    fn eval_fib() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "(define = (lambda (a b) (if (<= a b) (<= b a) false)))"
                => Ok(_),
            "(define fib (lambda (a b n) (if (= n 0.0) a (fib b (+ a b) (- n 1.0)))))"
                => Ok(_),
            "(define fib_bad (lambda (n) (if (= n 0.0) 0.0 (if (= n 1.0) 1.0 (+ (fib_bad (- n 1.0)) (fib_bad (- n 2.0)))))))"
                => Ok(_),
            "(fib 0. 1. 10.)"
                => Ok(Expr::Lit(Lit::Float(55.))),
            "(fib_bad 10.)"
                => Ok(Expr::Lit(Lit::Float(55.))),
        );

        Ok(())
    }

    #[test]
    fn eval_curried() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "(define f (lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))))"
                => Ok(_),
            "(((f 2.) 2.) 2.)"
                => Ok(Expr::Lit(Lit::Float(6.))),
        );

        Ok(())
    }

    #[test]
    fn eval_cons_as_lambda() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "(define cons (lambda (x y) (lambda (m) (m x y))))"
                => Ok(_),
            "(define car (lambda (z) (z (lambda (p q) p))))"
                => Ok(_),
            "(define cdr (lambda (z) (z (lambda (p q) q))))"
                => Ok(_),
            "(define list (cons 1.0 (cons 2. (cons 3. nil))))"
                => Ok(_),
            "(define sum (lambda (xs) (if (cdr xs) (+ (car xs) (sum (cdr xs))) (car xs))))"
                => Ok(_),
            "(sum list)"
                => Ok(Expr::Lit(Lit::Float(6.))),
        );

        Ok(())
    }

    #[test]
    fn eval_lits() -> Result<(), Error> {
        let mut env_table = EnvTable::with_builtins();

        assert_evals_from_str!(
            with env_table:
            "42."   => Ok(Expr::Lit(Lit::Float(42.))),
        );

        Ok(())
    }
}
