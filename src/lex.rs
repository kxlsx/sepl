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
        r"-?[0-9]+\.[0-9]*|-?[0-9]+(\.[0-9]*)?[eE][+\-]?[0-9]+",
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
        _ => unreachable!(),
    }
}

fn parse_float_token<'s>(lex: &mut Lexer<'s, Token<'s>>) -> f64 {
    unsafe { lex.slice().parse::<f64>().unwrap_unchecked() }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_token {
        ($lexer:ident, $token:expr) => {
            assert_eq!($lexer.next().expect("Lexer is empty!")?, $token);
        };
    }

    macro_rules! assert_empty {
        ($lexer:ident) => {
            assert_eq!($lexer.next(), None)
        };
    }

    macro_rules! assert_err {
        ($lexer:ident, $err:expr) => {
            assert_eq!($lexer.next().expect("Lexer is empty!"), Err($err));
        };
    }

    #[test]
    fn test_brakets() -> Result<(), Error> {
        let source = "([{}])";
        let mut lexer = Token::lexer(source);

        assert_token!(lexer, Token::LeftBracket(BracketType::Normal));
        assert_token!(lexer, Token::LeftBracket(BracketType::Square));
        assert_token!(lexer, Token::LeftBracket(BracketType::Curly));
        assert_token!(lexer, Token::RightBracket(BracketType::Curly));
        assert_token!(lexer, Token::RightBracket(BracketType::Square));
        assert_token!(lexer, Token::RightBracket(BracketType::Normal));
        assert_empty!(lexer);

        Ok(())
    }

    #[test]
    fn test_symbols() -> Result<(), Error> {
        let source = "skrzat\tchur-bo\nmc.flungus ;;; + __ - -abba";
        let mut lexer = Token::lexer(source);

        assert_token!(lexer, Token::Symbol("skrzat"));
        assert_token!(lexer, Token::Symbol("chur-bo"));
        assert_token!(lexer, Token::Symbol("mc.flungus"));
        assert_token!(lexer, Token::Symbol(";;;"));
        assert_token!(lexer, Token::Symbol("+"));
        assert_token!(lexer, Token::Symbol("__"));
        assert_token!(lexer, Token::Symbol("-"));
        assert_token!(lexer, Token::Symbol("-abba"));
        assert_empty!(lexer);

        Ok(())
    }

    #[test]
    fn test_symbols_bad() -> Result<(), Error> {
        let source = "-21. 12. true false nil ([{}]) 😎😎😎 形声形聲 ąęśćłżź";
        let mut lexer = Token::lexer(source);

        assert_token!(lexer, Token::Float(-21.));
        assert_token!(lexer, Token::Float(12.));
        assert_token!(lexer, Token::True);
        assert_token!(lexer, Token::False);
        assert_token!(lexer, Token::Nil);
        assert_token!(lexer, Token::LeftBracket(BracketType::Normal));
        assert_token!(lexer, Token::LeftBracket(BracketType::Square));
        assert_token!(lexer, Token::LeftBracket(BracketType::Curly));
        assert_token!(lexer, Token::RightBracket(BracketType::Curly));
        assert_token!(lexer, Token::RightBracket(BracketType::Square));
        assert_token!(lexer, Token::RightBracket(BracketType::Normal));
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_err!(lexer, Error::UnexpectedToken);
        assert_empty!(lexer);

        Ok(())
    }

    #[test]
    fn test_keywords() -> Result<(), Error> {
        let source = "true false nil";
        let mut lexer = Token::lexer(source);

        assert_token!(lexer, Token::True);
        assert_token!(lexer, Token::False);
        assert_token!(lexer, Token::Nil);
        assert_empty!(lexer);

        Ok(())
    }

    #[test]
    fn test_floats() -> Result<(), Error> {
        let source = "21. -37.42 0.1 1e-2 3E45 -15e10";
        let mut lexer = Token::lexer(source);

        assert_token!(lexer, Token::Float(21.));
        assert_token!(lexer, Token::Float(-37.42));
        assert_token!(lexer, Token::Float(0.1));
        assert_token!(lexer, Token::Float(1e-2));
        assert_token!(lexer, Token::Float(3e45));
        assert_token!(lexer, Token::Float(-15e10));
        assert_empty!(lexer);

        Ok(())
    }
}
