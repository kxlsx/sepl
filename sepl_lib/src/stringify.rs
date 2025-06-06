use crate::eval::{Error as EvalError, Expr, Procedure, Symbol, SymbolTable};

pub trait Resolver<S> {
    fn resolve(&self, symbol: S) -> &str;
}

pub trait Stringify<S, R: Resolver<S>> {
    fn stringify(&self, resolver: &SymbolTable) -> String;
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
            Expr::Call(head, tail) => {
                let head_str = head.stringify(symbol_table);
                let tail_str = tail.iter().fold(String::new(), |mut buf, expr| {
                    buf.push(' ');
                    buf.push_str(&expr.stringify(symbol_table));
                    buf
                });

                format!("({head_str}{tail_str})")
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
            }
            ref err @ EvalError::NotCallable { ref expr, .. } => {
                format!("<{}>: {}", expr.stringify(symbol_table), err)
            }
        }
    }
}
