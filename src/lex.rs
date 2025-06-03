use thiserror::Error;

pub use logos::Logos as Lex;

// TODO:
#[derive(Copy, Clone, Error, Debug, PartialEq, Default)]
pub enum Error {
    #[default]
    #[error("TODO: unexpected token")]
    UnexpectedToken,
}

#[derive(Lex, Debug, Clone, Copy, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(error = Error)]
pub enum Token<'i> {
    #[token("(")]
    LeftBracket,
    #[token(")")]
    RightBracket,
    #[regex(
        r"[0-9]+\.[0-9]*|[0-9]+(\.[0-9]*)?[eE][+\-]?[0-9]+",
        |lex| lex.slice().parse::<f64>().expect("TODO: ?????"),
        priority = 2
    )]
    Float(f64),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("nil")]
    Nil,
    #[regex(r"[[:alpha:][:punct:]--)--(]+")]
    Symbol(&'i str),
}
