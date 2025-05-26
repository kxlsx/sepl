use logos::Logos;
use sepl::eval::{Builtin, Env, EnvTable};
use sepl::eval::{Expr, Symbol};
use sepl::lex::Token;
use sepl::parse::{Parse, Parser};

const COD: &str = "
    (define pi  (- 4.1415 1.0))
    (/ ((lambda x (* x x)) pi) 6.)
    (define > (lambda a b (if (<= a b) false true)))

    (define = (lambda a b (if (<= a b) (<= b a) false)))

    (define fact (lambda n (if (= n 0.0) 1. (* n (fact (- n 1.))))))
    (fact 30.)
    fact
    
    (define fib_bad (lambda n (if (= n 0.0) 0.0 (if (= n 1.0) 1.0 (+ (fib_bad (- n 1.0)) (fib_bad (- n 2.0)))))))
  
    (define fib (lambda a b n (if (= n 0.0) a (fib b (+ a b) (- n 1.0)))))

    (fib 0.0 1.0 30.)

    (define cons (lambda x y (lambda m (m x y))))
    (define car (lambda z (z (lambda p q p))))
    (define cdr (lambda z (z (lambda p q q))))

    (car (cons 1.0 (cons 2. (cons 3. nil))))

    (define f (lambda a (lambda b (lambda c (+ a (+ b c))))))
    (((f 2.) 2.) 2.)
";

fn main() {
    let mut env_table: EnvTable<'_> = EnvTable::new();
    env_table.define_global_symbol(Symbol::from("lambda"), Expr::Builtin(Builtin::Lambda));
    env_table.define_global_symbol(Symbol::from("define"), Expr::Builtin(Builtin::Define));
    env_table.define_global_symbol(Symbol::from("quote"), Expr::Builtin(Builtin::Quote));
    env_table.define_global_symbol(Symbol::from("eval"), Expr::Builtin(Builtin::Eval));
    env_table.define_global_symbol(Symbol::from("if"), Expr::Builtin(Builtin::IfElse));
    env_table.define_global_symbol(Symbol::from("<="), Expr::Builtin(Builtin::Leq));
    env_table.define_global_symbol(Symbol::from("+"), Expr::Builtin(Builtin::Add));
    env_table.define_global_symbol(Symbol::from("-"), Expr::Builtin(Builtin::Sub));
    env_table.define_global_symbol(Symbol::from("*"), Expr::Builtin(Builtin::Mul));
    env_table.define_global_symbol(Symbol::from("/"), Expr::Builtin(Builtin::Div));

    let lex = Token::lexer(COD);
    let mut parser = Parser::new(lex.map(|t| t.expect("unexpected token")));

    while let Ok(expr) = Expr::parse(&mut parser) {
        let res = expr.eval(&mut env_table, Env::global());

        match res {
            Ok(e) => println!("{}", e),
            Err(e) => println!("{}", e),
        }
    }
}
