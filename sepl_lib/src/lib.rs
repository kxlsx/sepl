//! Library containing ways to [`Parse`](parse::Parser)
//! an [`evaluate`](eval::Expr::eval) `sepl`
//! expressions, stored as [`Expr`](eval::Expr)s.
// TODO: more docs here
// TODO: UPDATE DOCS WITH NEW INTERNING!!!!

/// Module containing building blocks
/// of the `sepl` language and ways
/// to evaluate expressions.
///
/// Mostly it contains:
/// [`Expr`](eval::Expr),
/// [`SymbolTable`](eval::SymbolTable)
/// and [`EnvTable`](eval::EnvTable).
pub mod eval;
/// Module containing the [`Lexer`](lex::Lexer) yielding [`Token`](lex::Token)s `sepl` language.
pub mod lex;
/// Module containing the [`Parser`](parse::Parser) for the `sepl` language.
pub mod parse;
/// Module containing functions used to turn [`Expr`](eval::Expr)s into [`String`]s.
pub mod stringify;
