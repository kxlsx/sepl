use core::result::Result as StdResult;
use thiserror::Error;

pub type Result<T> = StdResult<T, Error>;

// TODO:
#[derive(Error, Debug)]
pub enum Error {
    #[error("TODO: undefined symbol")]
    UndefinedSymbol,
    #[error("TODO: environment redefinition")]
    RedefinedEnv,
    #[error("TODO: wrong arg count")]
    IncorrectArgCount,
    #[error("TODO: wrong arg type")]
    IncorrectArgType,
    #[error("TODO: not callable")]
    NotCallable
}

