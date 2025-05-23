use logos::Logos;

#[derive(Logos, Debug, Clone, Copy, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'i> {
    #[token("(")]
    LeftBracket,
    #[token(")")]
    RightBracket,
    #[regex(
        r"[0-9]+\.[0-9]*|[0-9]+(\.[0-9]*)?[eE][+\-]?[0-9]+",
        |lex| lex.slice().parse::<f32>().expect("TODO: ?????"),
        priority = 2
    )]
    Float(f32),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"[[:alpha:][:punct:]--)--(]+")]
    Symbol(&'i str),
}

#[macro_export]
macro_rules! token {
    ($token:tt$(($val:expr))?) => {
        lisprust::lex::Token::$token$(($val))?
    };
    [$($token:tt$(($val:expr))?),*] => {
        vec![$(lisprust::lex::Token::$token$(($val))?),*]
    }
}
pub use token;
