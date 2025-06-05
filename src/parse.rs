use std::error::Error as ErrorTrait;
use std::iter::Peekable;

use logos::Logos;
use thiserror::Error;

use crate::eval::{Expr, Lit, Symbol, SymbolTable};
use crate::lex::{Error as LexError, Token};

#[derive(Error, Debug, PartialEq, Eq, Hash)]
pub enum Error {
    #[error("Expected a symbol, found: '{found}'.")]
    ExpectedSymbol { found: String },
    #[error("Expected a literal, found: '{found}'.")]
    ExpectedLit { found: String },
    #[error("Expressions cannot be empty.")]
    ExpectedExpr,
    #[error("Expected '(', '[', or '}}', found: '{found}'")]
    ExpectedLeftBracket { found: String },
    #[error("Expected a matching '{expected}', found '{found}'")]
    ExpectedRightBracket { expected: String, found: String },
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Unexpected Token")]
    UnexpectedToken,
}

impl From<LexError> for Error {
    fn from(value: LexError) -> Self {
        match value {
            LexError::UnexpectedToken => Error::UnexpectedToken,
        }
    }
}

pub struct Parser<'s, 'i, E, L>
where
    E: ErrorTrait + Into<Error> + Copy + Clone,
    L: Iterator<Item = Result<Token<'i>, E>>,
{
    lexer: Peekable<L>,
    symbol_table: &'s mut SymbolTable,
}

impl<'s, 'i, E, L> Parser<'s, 'i, E, L>
where
    E: ErrorTrait + Into<Error> + Copy + Clone,
    L: Iterator<Item = Result<Token<'i>, E>>,
{
    pub fn new(lexer: L, symbol_table: &'s mut SymbolTable) -> Self {
        Parser {
            lexer: lexer.peekable(),
            symbol_table,
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr, Error> {
        Expr::parse(self)
    }

    pub fn parse_symbol(&mut self) -> Result<Symbol, Error> {
        Symbol::parse(self)
    }

    pub fn parse_lit(&mut self) -> Result<Lit, Error> {
        Lit::parse(self)
    }

    pub fn lookahead(&mut self) -> Option<Result<Token<'i>, E>> {
        self.lexer.peek().copied()
    }

    pub fn eat(&mut self) -> Option<Result<Token<'i>, E>> {
        self.lexer.next()
    }

    pub fn eat_if_eq(&mut self, expected: Token<'i>) -> Option<Result<Token<'i>, E>> {
        self.lexer
            .next_if(|lex_res| lex_res.is_ok_and(|token| token == expected))
    }
}

impl<'s, 'i, E, L> Iterator for Parser<'s, 'i, E, L>
where
    E: ErrorTrait + Into<Error> + Copy + Clone,
    L: Iterator<Item = Result<Token<'i>, E>>,
{
    type Item = Result<Expr, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lookahead().is_none() {
            None
        } else {
            Some(self.parse_expr())
        }
    }
}

pub trait ParseFrom<T>
where
    Self: Sized,
{
    fn parse_from(value: T, symbol_table: &mut SymbolTable) -> Result<Self, Error>;
}

impl ParseFrom<&str> for Expr {
    fn parse_from(value: &str, symbol_table: &mut SymbolTable) -> Result<Self, Error> {
        Expr::parse(&mut Parser::new(Token::lexer(value), symbol_table))
    }
}

impl ParseFrom<&str> for Symbol {
    fn parse_from(value: &str, symbol_table: &mut SymbolTable) -> Result<Self, Error> {
        Symbol::parse(&mut Parser::new(Token::lexer(value), symbol_table))
    }
}

impl ParseFrom<&str> for Lit {
    fn parse_from(value: &str, symbol_table: &mut SymbolTable) -> Result<Self, Error> {
        Lit::parse(&mut Parser::new(Token::lexer(value), symbol_table))
    }
}

pub trait Parse
where
    Self: Sized,
{
    fn parse<'s, 'i, E, L>(parser: &mut Parser<'s, 'i, E, L>) -> Result<Self, Error>
    where
        E: ErrorTrait + Into<Error> + Copy + Clone,
        L: Iterator<Item = Result<Token<'i>, E>>;
}

impl Parse for Symbol {
    fn parse<'s, 'i, E, L>(parser: &mut Parser<'s, 'i, E, L>) -> Result<Self, Error>
    where
        E: ErrorTrait + Into<Error> + Copy + Clone,
        L: Iterator<Item = Result<Token<'i>, E>>,
    {
        match parser.lookahead() {
            Some(Ok(Token::Symbol(name))) => {
                parser.eat();
                Ok(parser.symbol_table.intern(name))
            }
            Some(Err(lex_error)) => Err(lex_error.into()),
            None => Err(Error::UnexpectedEOF),
            Some(Ok(other_token)) => Err(Error::ExpectedSymbol {
                found: other_token.to_string(),
            }),
        }
    }
}

impl Parse for Lit {
    fn parse<'s, 'i, E, L>(parser: &mut Parser<'s, 'i, E, L>) -> Result<Self, Error>
    where
        E: ErrorTrait + Into<Error> + Copy + Clone,
        L: Iterator<Item = Result<Token<'i>, E>>,
    {
        let lit = match parser.lookahead() {
            Some(Ok(Token::Float(float))) => Ok(Lit::Float(float)),
            Some(Ok(Token::True)) => Ok(Lit::Bool(true)),
            Some(Ok(Token::False)) => Ok(Lit::Bool(false)),
            Some(Ok(Token::Nil)) => Ok(Lit::Nil),
            Some(Err(lex_error)) => Err(lex_error.into()),
            None => Err(Error::UnexpectedEOF),
            Some(Ok(other_token)) => Err(Error::ExpectedLit {
                found: other_token.to_string(),
            }),
        }?;

        parser.eat();
        Ok(lit)
    }
}

impl Parse for Expr {
    fn parse<'s, 'i, E, L>(parser: &mut Parser<'s, 'i, E, L>) -> Result<Self, Error>
    where
        E: ErrorTrait + Into<Error> + Copy + Clone,
        L: Iterator<Item = Result<Token<'i>, E>>,
    {
        if let Ok(symbol) = Symbol::parse(parser) {
            return Ok(Expr::Symbol(symbol));
        }

        if let Ok(lit) = Lit::parse(parser) {
            return Ok(Expr::Lit(lit));
        }

        let matching_bracket = match parser.lookahead() {
            Some(Ok(Token::LeftBracket(bracket_type))) => Token::RightBracket(bracket_type),
            Some(Err(lex_error)) => return Err(lex_error.into()),
            None => return Err(Error::UnexpectedEOF),
            Some(Ok(other_token)) => {
                return Err(Error::ExpectedLeftBracket {
                    found: other_token.to_string(),
                })
            }
        };

        parser.eat();

        let head = 
            if parser.eat_if_eq(matching_bracket).is_none() {
                Box::new(Expr::parse(parser).map_err(|err| match err {
                    Error::ExpectedLeftBracket { found } => Error::ExpectedRightBracket {
                        found,
                        expected: matching_bracket.to_string(),
                    },
                    _ => err,
                })?)
            } else {
                return Err(Error::ExpectedExpr)
            };

        let mut body = Vec::new();
        while parser.eat_if_eq(matching_bracket).is_none() {
            body.push(Expr::parse(parser).map_err(|err| match err {
                Error::ExpectedLeftBracket { found } => Error::ExpectedRightBracket {
                    found,
                    expected: matching_bracket.to_string(),
                },
                _ => err,
            })?);
        }

        Ok(Expr::Call(head, body))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_symbol {
        ($symbol:expr) => {
            match $symbol {
                Expr::Symbol(_) => (),
                _ => panic!("Is not a symbol!"),
            }
        };
    }

    macro_rules! assert_expr_call {
        ({ head: $head_predicate:expr, tail: [ $($tail_predicate:expr),* ] } = $exp:expr) => {
            if let Expr::Call(head, tail) = $exp {
                $head_predicate(*head);

                let mut tailiter = tail.into_iter();
                $(
                    $tail_predicate(tailiter.next().expect("Tail is shorter than expected!"));
                )*
                if !tailiter.next().is_none() { panic!("Tail is longer than expected!") }
            } else  { panic!("Parsed something that's not a call!") } 
        }
    }

    #[test]
    fn parse_nested() -> Result<(), Error> {
        let source = "(Tato mój (w Świnoujsciu żył))";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_expr_call!({
            head: |h| assert_symbol!(h), 
            tail: [
                |t| assert_symbol!(t), 
                |call| assert_expr_call!({
                    head: |h| assert_symbol!(h), 
                    tail: [
                        |t| assert_symbol!(t),
                        |t| assert_symbol!(t)
                    ]
                } = call)
            ]
        } = Expr::parse(&mut parser)?);

        Ok(())
    }

    #[test]
    fn parse_lits() -> Result<(), Error> {
        let source = "true false nil 1.443 -1.12 -4231. 1e-10 -15e10";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(
            Expr::parse(&mut parser)?,
            Expr::Lit(Lit::Bool(true))
        );

        assert_eq!(
            Expr::parse(&mut parser)?,
            Expr::Lit(Lit::Bool(false))
        );

        assert_eq!(
            Expr::parse(&mut parser)?,
            Expr::Lit(Lit::Nil)
        );

        assert_eq!(
            Expr::parse(&mut parser)?,
            Expr::Lit(Lit::Float(1.443))
        );

        assert_eq!(
            Expr::parse(&mut parser)?,
            Expr::Lit(Lit::Float(-1.12))
        );

        assert_eq!(
            Expr::parse(&mut parser)?,
            Expr::Lit(Lit::Float(-4231.))
        );

        assert_eq!(
            Expr::parse(&mut parser)?,
            Expr::Lit(Lit::Float(1e-10))
        );

        assert_eq!(
            Expr::parse(&mut parser)?,
            Expr::Lit(Lit::Float(-15e10))
        );

        Ok(())
    }

    #[test]
    fn parse_err_symbol() -> Result<(), Error> {
        let source = "true false 8000. ()[]{}";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from("true") })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from("false") })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from("8000") })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from("(") })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from(")") })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from("[") })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from("]") })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from("{") })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol { found: String::from("}") })
        );
        parser.eat();

        Ok(())
    }

    #[test]
    fn parse_err_expected_expr() -> Result<(), Error> {
        let source = "(function arg1 ())";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(
            Expr::parse(&mut parser),
            Err(Error::ExpectedExpr),
        );

        Ok(())
    }

    #[test]
    fn parse_err_expected_left() -> Result<(), Error> {
        let source = ") lorem ipsum";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(
            Expr::parse(&mut parser),
            Err(Error::ExpectedLeftBracket { found: String::from(")") }),
        );

        Ok(())
    }

    #[test]
    fn parse_err_expected_right() -> Result<(), Error> {
        let source = "(lorem ipsum(dolor] )";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(
            Expr::parse(&mut parser),
            Err(Error::ExpectedRightBracket { expected: String::from(")"), found: String::from("]") }),
        );

        Ok(())
    }

    #[test]
    fn parse_err_unexpected_eof() -> Result<(), Error> {
        let source = "(lorem ipsum(dolor)";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(
            Expr::parse(&mut parser),
            Err(Error::UnexpectedEOF),
        );

        Ok(())
    }

    #[test]
    fn parse_empty() -> Result<(), Error> {
        let source = "";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

         assert_eq!(
            Expr::parse(&mut parser),
            Err(Error::UnexpectedEOF),
        );

        Ok(())
    }
}
