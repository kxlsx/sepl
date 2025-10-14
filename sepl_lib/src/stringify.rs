use crate::eval::{Error as EvalError, Expr, Procedure, Resolve, Symbol};

use itertools::Itertools;

/// Trait representing a type that can
/// be turned into a [`String`] using
/// a struct implementing [`Resolve`].
pub trait Stringify<S, R: Resolve<S>> {
    fn stringify(&self, resolver: &R) -> String;
}

impl<R: Resolve<Symbol>> Stringify<Symbol, R> for Expr {
    fn stringify(&self, symbol_interner: &R) -> String {
        match self {
            Expr::Symbol(symbol) => String::from(symbol_interner.resolve(*symbol)),
            Expr::Lit(lit) => lit.to_string(),
            Expr::Builtin(builtin) => builtin.to_string(),
            Expr::Procedure(proc) => proc.stringify(symbol_interner),
            Expr::List(list) => {
                format!(
                    "({})",
                    list.iter()
                        .map(|expr| expr.stringify(symbol_interner))
                        .format(" ")
                )
            }
        }
    }
}

impl<R: Resolve<Symbol>> Stringify<Symbol, R> for Procedure {
    fn stringify(&self, symbol_interner: &R) -> String {
        let args_str = format!(
            "({})",
            self.params()
                .iter()
                .map(|symbol| symbol_interner.resolve(*symbol))
                .format(" ")
        );
        let body_str = self.body().stringify(symbol_interner);

        format!("(lambda {args_str} {body_str})")
    }
}

impl<R: Resolve<Symbol>> Stringify<Symbol, R> for EvalError {
    fn stringify(&self, symbol_interner: &R) -> String {
        match self {
            ref err @ EvalError::IncorrectArgCount { ref expr, .. } => {
                format!("<{}>: {}", expr.stringify(symbol_interner), err)
            }
            err @ EvalError::IncorrectArgType { builtin, .. } => {
                format!("<{}>: {}", builtin, err)
            }
            err @ EvalError::DivisionByZero { builtin, .. } => {
                format!("<{}>: {}", builtin, err)
            }
        }
    }
}
