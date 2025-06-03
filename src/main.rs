use logos::Logos;
use sepl::eval::Expr;
use sepl::eval::{Builtin, EvalTable, SymbolTable};
use sepl::lex::Token;
use sepl::parse::Parser;

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

    (define list (cons 1.0 (cons 2. (cons 3. nil))))

    (define f (lambda a (lambda b (lambda c (+ a (+ b c))))))
    (((f 2.) 2.) 2.)

    (define sum (lambda xs (if (cdr xs) (+ (car xs) (sum (cdr xs))) (car xs))))
    (sum list)
";

fn main() {
    let mut symbol_table = SymbolTable::new();

    let mut env_table: EvalTable = EvalTable::new();
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Lambda.to_string()),
        Expr::Builtin(Builtin::Lambda),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Define.to_string()),
        Expr::Builtin(Builtin::Define),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Quote.to_string()),
        Expr::Builtin(Builtin::Quote),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Eval.to_string()),
        Expr::Builtin(Builtin::Eval),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::IfElse.to_string()),
        Expr::Builtin(Builtin::IfElse),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Leq.to_string()),
        Expr::Builtin(Builtin::Leq),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Add.to_string()),
        Expr::Builtin(Builtin::Add),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Sub.to_string()),
        Expr::Builtin(Builtin::Sub),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Mul.to_string()),
        Expr::Builtin(Builtin::Mul),
    );
    env_table.symbol_define_global(
        symbol_table.intern(&Builtin::Div.to_string()),
        Expr::Builtin(Builtin::Div),
    );

    let lex = Token::lexer(COD);
    let parser = Parser::new(lex, &mut symbol_table);

    let env_global = env_table.env_global();
    for parsed_expr in parser {
        if let Err(e) = parsed_expr {
            println!("{}", e);
            continue;
        }
        let expr = parsed_expr.unwrap();

        let evaluated_expr = expr.eval(&mut env_table, env_global);
        match evaluated_expr {
            Ok(e) => println!("{:?}", e),
            Err(e) => println!("{}", e),
        }
    }
}
