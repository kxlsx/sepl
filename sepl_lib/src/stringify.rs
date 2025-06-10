use crate::eval::{Error as EvalError, Expr, Procedure, Symbol, SymbolTable};

/// Trait representing a type
/// that can resolve other types (`S`)
/// into [`&str`](str).
pub trait Resolver<S> {
    fn resolve(&self, symbol: S) -> &str;
}

/// Trait representing a type that can
/// be turned into a [`String`] using
/// a [`Resolver`].
pub trait Stringify<S, R: Resolver<S>> {
    fn stringify(&self, resolver: &R) -> String;
}

impl Resolver<Symbol> for SymbolTable {
    fn resolve(&self, symbol: Symbol) -> &str {
        SymbolTable::resolve(self, symbol)
    }
}

impl Stringify<Symbol, SymbolTable> for Expr {
    fn stringify(&self, symbol_table: &SymbolTable) -> String {
        match self {
            Expr::Symbol(symbol) => String::from(symbol_table.resolve(*symbol)),
            Expr::Lit(lit) => lit.to_string(),
            Expr::Builtin(builtin) => builtin.to_string(),
            Expr::Procedure(proc) => proc.stringify(symbol_table),
            Expr::List(list) => {
                let mut list_str = list.iter().fold(String::new(), |mut buf, expr| {
                    buf.push_str(&expr.stringify(symbol_table));
                    buf.push(' ');
                    buf
                });
                list_str.pop();

                format!("({list_str})")
            }
        }
    }
}

impl Stringify<Symbol, SymbolTable> for Procedure {
    fn stringify(&self, symbol_table: &SymbolTable) -> String {
        let args_str = self.params().iter().fold(String::new(), |mut buf, symbol| {
            buf.push(' ');
            buf.push_str(symbol_table.resolve(*symbol));

            buf
        });
        let body_str = self.body().stringify(symbol_table);

        format!("(lambda{args_str} {body_str})")
    }
}

impl Stringify<Symbol, SymbolTable> for EvalError {
    fn stringify(&self, symbol_table: &SymbolTable) -> String {
        match self {
            ref err @ EvalError::IncorrectArgCount { ref expr, .. } => {
                format!("<{}>: {}", expr.stringify(symbol_table), err)
            }
            err @ EvalError::IncorrectArgType { builtin, .. } => {
                format!("<{}>: {}", builtin, err)
            },
        }
    }
}
