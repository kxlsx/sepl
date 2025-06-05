use crate::eval::{Error as EvalError, Expr, Procedure, SymbolTable};

pub trait Stringify {
    fn stringify(&self, symbol_table: &SymbolTable) -> String;
}

impl Stringify for Expr {
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

impl Stringify for Procedure {
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

impl Stringify for EvalError {
    fn stringify(&self, symbol_table: &SymbolTable) -> String {
        match self {
            ref err @ EvalError::IncorrectArgCount { ref expr, .. } => {
                format!("'{}': {}", expr.stringify(symbol_table), err)
            }
            err @ EvalError::IncorrectArgType { builtin, .. } => {
                format!("'{}': {}", builtin, err)
            }
            ref err @ EvalError::NotCallable { ref expr, .. } => {
                format!("'{}': {}", expr.stringify(symbol_table), err)
            }
        }
    }
}
