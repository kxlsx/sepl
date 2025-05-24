use sepl::expr::{Builtin, Env, EnvTable};
use sepl::parse::{Parser, Parse};
use sepl::expr::{Expr, Symbol};
use sepl::lex::Token;
use logos::Logos;

use std::env;
use std::io::{self, BufRead};

const COD: &str = "
    (define pi  (+ 3.1415 1.0))
    (/ ((lambda x (* x x)) pi) 6.)
    (define > (lambda a b (if (<= a b) false true)))
    (define = (lambda a b (if (<= a b) (<= b a) false)))

    (define fact (lambda n (if (= n 0.0) 1. (* n (fact (- n 1.))))))
    (fact 10.)
    
    (define fib (lambda n (if (= n 0.0) 0.0 (if (= n 1.0) 1.0 (+ (fib (- n 1.0)) (fib (- n 2.0)))))))
    (fib 20.0)
";

fn main() {
    let mut env_table: EnvTable<'_> = EnvTable::new();
    env_table.define_global_symbol(Symbol::from("lambda"), Expr::Builtin(Builtin::Lambda));
    env_table.define_global_symbol(Symbol::from("define"), Expr::Builtin(Builtin::Define));
    env_table.define_global_symbol(Symbol::from("if"), Expr::Builtin(Builtin::IfElse));
    env_table.define_global_symbol(Symbol::from("<="), Expr::Builtin(Builtin::Leq));
    env_table.define_global_symbol(Symbol::from("+"), Expr::Builtin(Builtin::Add));
    env_table.define_global_symbol(Symbol::from("-"), Expr::Builtin(Builtin::Sub));
    env_table.define_global_symbol(Symbol::from("*"), Expr::Builtin(Builtin::Mul));
    env_table.define_global_symbol(Symbol::from("/"), Expr::Builtin(Builtin::Div));


    let lex = Token::lexer(COD);
    let mut parser = Parser::new(lex.map(|t| t.expect("unexpected token")));

    while let Ok(expr) = Expr::parse(&mut parser) {
        println!("{:?}", expr);

        println!("{:?}\n", expr.eval(&mut env_table, Env::global()));
    }
}
