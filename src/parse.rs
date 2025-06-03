use std::collections::LinkedList;
use std::error::Error as ErrorTrait;
use std::iter::Peekable;

use logos::Logos;
use thiserror::Error;

use crate::eval::{Expr, Lit, Symbol, SymbolTable};
use crate::lex::{Error as LexError, Token};

// TODO:
#[derive(Error, Debug)]
pub enum Error {
    #[error("TODO: something something expected symbol")]
    ExpectedSymbol,
    #[error("TODO: expected literal")]
    ExpectedLit,
    #[error("TODO: expected expr")]
    ExpectedExpr,
    #[error("TODO: unexpected EOF")]
    UnexpectedEOF,
    #[error("TODO: unexpected Token")]
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
        Expr::parse(
            &mut Parser::new(Token::lexer(value), symbol_table)
        )
    }
}

impl ParseFrom<&str> for Symbol {
    fn parse_from(value: &str, symbol_table: &mut SymbolTable) -> Result<Self, Error> {
        Symbol::parse(
            &mut Parser::new(Token::lexer(value), symbol_table)
        )
    }
}

impl ParseFrom<&str> for Lit {
    fn parse_from(value: &str, symbol_table: &mut SymbolTable) -> Result<Self, Error> {
        Lit::parse(
            &mut Parser::new(Token::lexer(value), symbol_table)
        )
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
            _ => Err(Error::ExpectedSymbol),
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
            _ => Err(Error::ExpectedLit),
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

        match parser.lookahead() {
            Some(Ok(Token::LeftBracket)) => (),
            Some(Err(lex_error)) => return Err(lex_error.into()),
            None => return Err(Error::UnexpectedEOF),
            _ => return Err(Error::ExpectedExpr),
        }

        parser.eat();

        let head = Expr::parse(parser)?;
        let mut tail = LinkedList::new();

        while parser.eat_if_eq(Token::RightBracket).is_none() {
            tail.push_back(Expr::parse(parser)?);
        }

        Ok(Expr::Call(Box::new(head), tail))
    }
}
