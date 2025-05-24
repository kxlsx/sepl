use core::result::Result as StdResult;
use thiserror::Error;

pub type Result<T> = StdResult<T, Error>;

// TODO:
#[derive(Error, Debug)]
pub enum Error {
    #[error("TODO: something something expected symbol")]
    ExpectedSymbol,
    #[error("TODO: expected literal")]
    ExpectedLit,
    #[error("TODO: expected expr")]
    ExpectedExpr,
}
