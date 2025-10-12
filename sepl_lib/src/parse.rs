use std::collections::LinkedList;

use logos::Logos;
use thiserror::Error;

use crate::eval::{Expr, Intern, Lit, Symbol};
use crate::lex::{Error as LexError, Lexer, Token};

/// Error type returned by the [`Parser`].
#[derive(Error, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    #[error("Expected a symbol, found: '{found}'.")]
    ExpectedSymbol { found: String },
    #[error("Expected a literal, found: '{found}'.")]
    ExpectedLit { found: String },
    #[error("Expected '(', '[', or '}}', found: '{found}'.")]
    ExpectedLeftBracket { found: String },
    #[error("Expected a matching '{expected}', found '{found}'.")]
    ExpectedRightBracket { expected: String, found: String },
    #[error("Unexpected EOF.")]
    UnexpectedEOF,
    #[error("Unexpected token: '{token}'.")]
    UnexpectedToken { token: String },
}

/// Struct representing a parser for
/// the `sepl` lanugage. Used to
/// parse [`Expr`]s from a [`Lexer`],
/// inserting encountered symbols
/// into the passed string interner 
/// (struct implementing [`Intern`])
pub struct Parser<'s, 'i, I: Intern<Symbol>> {
    lexer: Lexer<'i, Token<'i>>,
    lookahead_token: Option<Option<Result<Token<'i>, LexError>>>,
    symbol_interner: &'s mut I,
}

impl<'s, 'i, I: Intern<Symbol>> Parser<'s, 'i, I> {
    /// Create a new [`Parser`].
    pub fn new(lexer: Lexer<'i, Token<'i>>, symbol_interner: &'s mut I) -> Self {
        Parser {
            lexer,
            symbol_interner,
            lookahead_token: None,
        }
    }

    /// Try to parse an [`Expr`].
    pub fn parse_expr(&mut self) -> Result<Expr, Error> {
        if let Ok(symbol) = self.parse_symbol() {
            return Ok(Expr::Symbol(symbol));
        }

        if let Ok(lit) = self.parse_lit() {
            return Ok(Expr::Lit(lit));
        }

        let matching_bracket = match self.lookahead() {
            Some(Ok(Token::LeftBracket(bracket_type))) => Token::RightBracket(bracket_type),
            Some(Ok(other_token)) => {
                return Err(Error::ExpectedLeftBracket {
                    found: other_token.to_string(),
                })
            }
            Some(Err(lex_error)) => return Err(lex_error),
            None => return Err(Error::UnexpectedEOF),
        };

        self.eat();

        let mut list = LinkedList::new();
        while self.eat_if_eq(matching_bracket).is_none() {
            list.push_back(self.parse_expr().map_err(|err| match err {
                Error::ExpectedLeftBracket { found } => Error::ExpectedRightBracket {
                    found,
                    expected: matching_bracket.to_string(),
                },
                _ => err,
            })?);
        }

        Ok(Expr::List(list))
    }

    /// Try to parse a [`Symbol`].
    pub fn parse_symbol(&mut self) -> Result<Symbol, Error> {
        match self.lookahead() {
            Some(Ok(Token::Symbol(name))) => {
                self.eat();
                Ok(self.symbol_interner.intern(name))
            }
            Some(Ok(other_token)) => Err(Error::ExpectedSymbol {
                found: other_token.to_string(),
            }),
            Some(Err(lex_error)) => Err(lex_error),
            None => Err(Error::UnexpectedEOF),
        }
    }

    /// Try to parse an [`Lit`].
    pub fn parse_lit(&mut self) -> Result<Lit, Error> {
        let lit = match self.lookahead() {
            Some(Ok(Token::Float(float))) => Ok(Lit::Float(float)),
            Some(Ok(other_token)) => Err(Error::ExpectedLit {
                found: other_token.to_string(),
            }),
            Some(Err(lex_error)) => Err(lex_error),
            None => Err(Error::UnexpectedEOF),
        }?;

        self.eat();
        Ok(lit)
    }

    /// Return the next [`Token`] without consuming it.
    pub fn lookahead(&mut self) -> Option<Result<Token<'i>, Error>> {
        self.lookahead_token()
    }

    /// Return the next [`Token`] and consume it.
    pub fn eat(&mut self) -> Option<Result<Token<'i>, Error>> {
        if let Some(lookahead) = self.lookahead_token.take() {
            self.parse_result_from_lex_result(lookahead)
        } else {
            self.next_token()
        }
    }

    /// Return the next [`Token`] and consume it, only if the token matches
    /// `expected`.
    pub fn eat_if_eq(&mut self, expected: Token<'i>) -> Option<Result<Token<'i>, Error>> {
        let lookahead = self.lookahead();

        match lookahead {
            Some(Ok(token)) if token == expected => self.eat(),
            _ => None,
        }
    }

    /// Return the next [`Token`]
    fn next_token(&mut self) -> Option<Result<Token<'i>, Error>> {
        let next = self.lexer.next();
        self.parse_result_from_lex_result(next)
    }

    /// Peek the next [`Token`].
    fn lookahead_token(&mut self) -> Option<Result<Token<'i>, Error>> {
        if self.lookahead_token.is_none() {
            self.lookahead_token = Some(self.lexer.next());
        }

        self.parse_result_from_lex_result(self.lookahead_token.unwrap())
    }

    /// Convert [`lex::Error`](LexError) to [`parse::Error`](Error).
    fn parse_result_from_lex_result(
        &self,
        lex_result: Option<Result<Token<'i>, LexError>>,
    ) -> Option<Result<Token<'i>, Error>> {
        match lex_result {
            None => None,
            Some(Ok(token)) => Some(Ok(token)),
            Some(Err(LexError::UnexpectedToken)) => Some(Err(Error::UnexpectedToken {
                token: String::from(self.lexer.slice()),
            })),
        }
    }
}

impl<'s, 'i, I: Intern<Symbol>> Iterator for Parser<'s, 'i, I> {
    type Item = Result<Expr, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lookahead().is_none() {
            None
        } else {
            Some(self.parse_expr())
        }
    }
}

/// Trait representing a type
/// that can be created using a [`Parser`].
pub trait Parse<I>
where
    Self: Sized,
    I: Intern<Symbol>
{
    fn parse(parser: &mut Parser<I>) -> Result<Self, Error>;
}

impl<I: Intern<Symbol>> Parse<I> for Symbol {
    fn parse(parser: &mut Parser<I>) -> Result<Self, Error> {
        parser.parse_symbol()
    }
}

impl<I: Intern<Symbol>> Parse<I> for Lit {
    fn parse(parser: &mut Parser<I>) -> Result<Self, Error> {
        parser.parse_lit()
    }
}

impl <I: Intern<Symbol>> Parse<I> for Expr {
    fn parse(parser: &mut Parser<I>) -> Result<Self, Error> {
        parser.parse_expr()
    }
}

/// Trait representing a type
/// that can be created from `T`.
pub trait ParseFrom<T, I: Intern<Symbol>>
where
    Self: Sized,
{
    fn parse_from(value: T, symbol_interner: &mut I) -> Result<Self, Error>;
}

impl<I: Intern<Symbol>> ParseFrom<&str, I> for Expr {
    fn parse_from(value: &str, symbol_interner: &mut I) -> Result<Self, Error> {
        Expr::parse(&mut Parser::new(Token::lexer(value), symbol_interner))
    }
}

impl<I: Intern<Symbol>> ParseFrom<&str, I> for Symbol {
    fn parse_from(value: &str, symbol_interner: &mut I) -> Result<Self, Error> {
        Symbol::parse(&mut Parser::new(Token::lexer(value), symbol_interner))
    }
}

impl<I: Intern<Symbol>> ParseFrom<&str, I> for Lit {
    fn parse_from(value: &str, symbol_interner: &mut I) -> Result<Self, Error> {
        Lit::parse(&mut Parser::new(Token::lexer(value), symbol_interner))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::eval::SymbolTable;

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
            if let Expr::List(mut list) = $exp {
                $head_predicate(list.pop_front().expect("Head does not exst"));

                let mut tailiter = list.into_iter();
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
        let source = "1.443 -1.12 -4231. 1e-10 -15e10";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(Expr::parse(&mut parser)?, Expr::Lit(Lit::Float(1.443)));

        assert_eq!(Expr::parse(&mut parser)?, Expr::Lit(Lit::Float(-1.12)));

        assert_eq!(Expr::parse(&mut parser)?, Expr::Lit(Lit::Float(-4231.)));

        assert_eq!(Expr::parse(&mut parser)?, Expr::Lit(Lit::Float(1e-10)));

        assert_eq!(Expr::parse(&mut parser)?, Expr::Lit(Lit::Float(-15e10)));

        Ok(())
    }

    #[test]
    fn parse_err_symbol() -> Result<(), Error> {
        let source = "8000. ()[]{}";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol {
                found: String::from("8000")
            })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol {
                found: String::from("(")
            })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol {
                found: String::from(")")
            })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol {
                found: String::from("[")
            })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol {
                found: String::from("]")
            })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol {
                found: String::from("{")
            })
        );
        parser.eat();
        assert_eq!(
            Symbol::parse(&mut parser),
            Err(Error::ExpectedSymbol {
                found: String::from("}")
            })
        );
        parser.eat();

        Ok(())
    }

    #[test]
    fn parse_err_expected_left() -> Result<(), Error> {
        let source = ") lorem ipsum";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(
            Expr::parse(&mut parser),
            Err(Error::ExpectedLeftBracket {
                found: String::from(")")
            }),
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
            Err(Error::ExpectedRightBracket {
                expected: String::from(")"),
                found: String::from("]")
            }),
        );

        Ok(())
    }

    #[test]
    fn parse_err_unexpected_eof() -> Result<(), Error> {
        let source = "(lorem ipsum(dolor)";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(Expr::parse(&mut parser), Err(Error::UnexpectedEOF),);

        Ok(())
    }

    #[test]
    fn parse_empty() -> Result<(), Error> {
        let source = "";

        let mut symbol_table = SymbolTable::new();
        let mut parser = Parser::new(Token::lexer(source), &mut symbol_table);

        assert_eq!(Expr::parse(&mut parser), Err(Error::UnexpectedEOF),);

        Ok(())
    }
}
