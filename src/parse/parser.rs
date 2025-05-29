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

    pub fn parse_expr(&mut self) -> Result<Expr> {
        Expr::parse(self)
    }

    pub fn parse_symbol(&mut self) -> Result<Symbol> {
        Symbol::parse(self)
    }

    pub fn parse_lit(&mut self) -> Result<Lit> {
        Lit::parse(self)
    }

    fn lookahead(&mut self) -> Option<Token<'i>> {
        Some(*self.lexer.peek()?)
    }

    fn eat(&mut self) -> Option<Token<'i>> {
        self.lexer.next()
    }

    fn eat_if_eq(&mut self, expected: &Token<'i>) -> Option<Token<'i>> {
        self.lexer.next_if_eq(expected)
    }
}

impl<'s, 'i, L: Iterator<Item = Token<'i>>> Iterator for Parser<'s, 'i, L> {
    type Item = Result<Expr>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lookahead().is_none() {
            None
        } else {
            Some(self.parse_expr())
        }
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
        match parser.lookahead() {
            Some(Token::Symbol(name)) => {
                parser.eat();
                Ok(parser.symbol_table.intern(name))
            },
            None => Err(Error::UnexpectedEOF),
            _ => Err(Error::ExpectedSymbol)
        }
    }
}

impl Parse for Lit {
    fn parse<'s, 'i, L: Iterator<Item = Token<'i>>>(parser: &mut Parser<'s, 'i, L>) -> Result<Self> {
        let lit = match parser.lookahead() {
            Some(Token::Float(float)) => Ok(Lit::Float(float)),
            Some(Token::True) => Ok(Lit::Bool(true)),
            Some(Token::False) => Ok(Lit::Bool(false)),
            Some(Token::Nil) => Ok(Lit::Nil),
            None => Err(Error::UnexpectedEOF),
            _ => Err(Error::ExpectedLit),
        }?;

        parser.eat();
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

        match parser.lookahead() {
            Some(Token::LeftBracket) => (),
            None => return Err(Error::UnexpectedEOF),
            _ => return Err(Error::ExpectedExpr),
        }

        parser.eat();

        let head = Expr::parse(parser)?;
        let mut tail = LinkedList::new();
    
        while parser.eat_if_eq(&Token::RightBracket).is_none() {
            tail.push_back(Expr::parse(parser)?);
        }

        Ok(Expr::Call(Box::new(head), tail))
    }
}
