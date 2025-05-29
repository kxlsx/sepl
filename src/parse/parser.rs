use std::collections::LinkedList;
use std::iter::Peekable;

use super::error::{Error, Result};

use crate::eval::{Expr, Lit, Symbol, SymbolTable};
use crate::lex::Token;

pub struct Parser<'s, 'i, L: Iterator<Item = Token<'i>>> {
    lexer: Peekable<L>,
    symbol_table: &'s mut SymbolTable,
}

impl<'s, 'i, L: Iterator<Item = Token<'i>>> Parser<'s, 'i, L> {
    pub fn new(lexer: L, symbol_table: &'s mut SymbolTable) -> Self {
        Parser {
            lexer: lexer.peekable(),
            symbol_table
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

pub trait Parse
where
    Self: Sized,
{
    fn parse<'s, 'i, L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'s, 'i, L>) -> Result<Self>;
}


impl Parse for Symbol {
    fn parse<'s, 'i, L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'s, 'i, L>) -> Result<Self> {
        if let Some(Token::Symbol(name)) = parser.peek_token() {
            parser.next_token();
            Ok(parser.symbol_table.intern(name))
        } else {
            Err(Error::ExpectedSymbol)
        }
    }
}

impl Parse for Lit {
    fn parse<'s, 'i, L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'s, 'i, L>) -> Result<Self> {
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

impl Parse for Expr {
    fn parse<'s, 'i, L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'s, 'i, L>) -> Result<Self> {
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
