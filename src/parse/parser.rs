use std::collections::LinkedList;
use std::iter::Peekable;

use super::error::{Error, Result};

use crate::eval::{Expr, Lit, Symbol};
use crate::lex::Token;

pub struct Parser<'i, L: Iterator<Item = Token<'i>>> {
    lexer: Peekable<L>,
}

impl<'i, L: Iterator<Item = Token<'i>>> Parser<'i, L> {
    pub fn new(lexer: L) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    pub fn peek_token(&mut self) -> Option<Token<'i>> {
        Some(self.lexer.peek()?.to_owned())
    }

    pub fn next_token(&mut self) -> Option<Token<'i>> {
        self.lexer.next()
    }

    pub fn next_token_if(
        &mut self,
        predicate: impl FnOnce(&Token<'i>) -> bool,
    ) -> Option<Token<'i>> {
        self.lexer.next_if(predicate)
    }

    pub fn next_token_if_eq(&mut self, expected: &Token<'i>) -> Option<Token<'i>> {
        self.lexer.next_if_eq(expected)
    }
}

impl<'i> Parse<'i> for Symbol<'i> {
    fn parse<L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'i, L>) -> Result<Self> {
        if let Some(Token::Symbol(name)) = parser.peek_token() {
            parser.next_token();
            Ok(Symbol::from(name))
        } else {
            Err(Error::ExpectedSymbol)
        }
    }
}

impl<'i> Parse<'i> for Lit {
    fn parse<L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'i, L>) -> Result<Self> {
        let lit = match parser.peek_token() {
            Some(Token::Float(float)) => Ok(Lit::Float(float)),
            Some(Token::True) => Ok(Lit::Bool(true)),
            Some(Token::False) => Ok(Lit::Bool(false)),
            Some(Token::Nil) => Ok(Lit::Nil),
            _ => Err(Error::ExpectedLit),
        }?;

        parser.next_token();
        Ok(lit)
    }
}

pub trait Parse<'i>
where
    Self: Sized,
{
    fn parse<L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'i, L>) -> Result<Self>;
}

impl<'i> Parse<'i> for Expr<'i> {
    fn parse<L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'i, L>) -> Result<Self> {
        if let Ok(symbol) = Symbol::parse(parser) {
            return Ok(Expr::Symbol(symbol));
        }

        if let Ok(lit) = Lit::parse(parser) {
            return Ok(Expr::Lit(lit));
        }

        if parser.next_token_if_eq(&Token::LeftBracket).is_some() {
            let head = Expr::parse(parser)?;
            let mut tail = LinkedList::new();

            while parser.next_token_if_eq(&Token::RightBracket).is_none() {
                tail.push_back(Expr::parse(parser)?);
            }

            Ok(Expr::Call(Box::new(head), tail))
        } else {
            Err(Error::ExpectedExpr)
        }
    }
}
