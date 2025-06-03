use thiserror::Error;

// TODO:
#[derive(Error, Debug)]
pub enum Error {
    #[error("TODO: wrong arg count")]
    IncorrectArgCount,
    #[error("TODO: wrong arg type")]
    IncorrectArgType,
    #[error("TODO: not callable")]
    NotCallable,
}
