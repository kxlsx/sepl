use thiserror::Error;

// TODO: better errors
#[derive(Error, Debug)]
pub enum Error {
    #[error("TODO: wrong arg count")]
    IncorrectArgCount,
    #[error("TODO: wrong arg type")]
    IncorrectArgType,
    #[error("TODO: not callable")]
    NotCallable,
}
