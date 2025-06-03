use sepl::eval::{EvalTable, SymbolTable};
use sepl::lex::{Token, Lex};
use sepl::parse::Parser;

const COD: &str = "
    (define pi (- 4.1415 1.0))
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

    ((lambda (do (define a 10.) (define b 2.) (+ a b) )))
";

fn main() {
    let mut symbol_table = SymbolTable::new();
    let mut eval_table = EvalTable::with_builtins(&mut symbol_table);

    let lex = Token::lexer(COD);
    let parser = Parser::new(lex, &mut symbol_table);

    let env_global = eval_table.env_global();
    for parsed_expr in parser {
        if let Err(e) = parsed_expr {
            println!("{}", e);
            continue;
        }
        let expr = parsed_expr.unwrap();

        let evaluated_expr = expr.eval(&mut eval_table, env_global);
        match evaluated_expr {
            Ok(e) => println!("{:?}", e),
            Err(e) => println!("{}", e),
        }
    }
}
