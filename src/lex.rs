use std::fmt::Display;

use thiserror::Error;

use logos::Lexer;

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
    #[regex(r"[\(\[\{]", parse_bracket_token)]
    LeftBracket(BracketType),
    #[regex(r"[\)\]\}]", parse_bracket_token)]
    RightBracket(BracketType),
    #[regex(
        r"[0-9]+\.[0-9]*|[0-9]+(\.[0-9]*)?[eE][+\-]?[0-9]+",
        parse_float_token,
        priority = 2
    )]
    Float(f64),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("nil")]
    Nil,
    #[regex(r"[[:alpha:][:punct:]--(--)--\[--\]--{--}]+")]
    Symbol(&'i str),
}

impl<'i> Display for Token<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Token::LeftBracket(BracketType::Normal) => write!(f, "("),
                Token::LeftBracket(BracketType::Square) => write!(f, "["),
                Token::LeftBracket(BracketType::Curly) => write!(f, "{{"),
                Token::RightBracket(BracketType::Normal) => write!(f, ")"),
                Token::RightBracket(BracketType::Square) => write!(f, "]"),
                Token::RightBracket(BracketType::Curly) => write!(f, "}}"),
                Token::Float(float) => write!(f, "{}", float),
                Token::True => write!(f, "true"),
                Token::False => write!(f, "false"),
                Token::Nil => write!(f, "nil"),
                Token::Symbol(name) => write!(f, "{}", name), 
            }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BracketType {
    Normal,
    Square,
    Curly,
}

fn parse_bracket_token<'s>(lex: &mut Lexer<'s, Token<'s>>) -> BracketType {
    match lex.slice().chars().next().expect("TODO: ????") {
        '(' | ')' => BracketType::Normal,
        '[' | ']' => BracketType::Square,
        '{' | '}' => BracketType::Curly,
        _ => unreachable!()
    }
}

fn parse_float_token<'s>(lex: &mut Lexer<'s, Token<'s>>) -> f64 {
    lex.slice().parse::<f64>().expect("TODO: ?????")
}