use thiserror::Error;

use super::{Builtin, Expr};

/// Error type returned by [`Expr::eval`],
/// [`Builtin::eval`] and [`Procedure::eval`](super::Procedure::eval).
#[derive(Error, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    #[error("Expression expects <{expected}> arguments, not <{found}>.")]
    IncorrectArgCount {
        expr: Expr,
        expected: usize,
        found: usize,
    },
    #[error("Expression expects argument of type <{expected}>, not <{found}>.")]
    IncorrectArgType {
        builtin: Builtin,
        expected: &'static str,
        found: &'static str,
    },
    #[error("Expression is not callable.")]
    NotCallable { expr: Expr },
}
