use sepl::eval::{EvalTable, SymbolTable};
use sepl::lex::{Lex, Token};
use sepl::parse::Parser;
use sepl::stringify::Stringify;

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

    [define list {cons 1.0 {cons 2. {cons 3. nil}}}]

    (define f (lambda a (lambda b (lambda c (+ a (+ b c))))))
    (((f 2.) 2.) 2.)

    (define sum (lambda xs (if (cdr xs) (+ (car xs) (sum (cdr xs))) (car xs))))
    (sum list)

    (define x x)
    
    (x)

    ((quote (+ 1. 0.)) (quote (x)) 2.)
    (lambda)

";

fn main() {
    let mut symbol_table = SymbolTable::new();

    let lex = Token::lexer(COD);
    let parser = Parser::new(lex, &mut symbol_table);

    let mut parsed_exprs = Vec::new();
    for expr in parser {
        if let Err(e) = expr {
            println!("{}", e);
            break;
        }
        parsed_exprs.push(expr.unwrap());
    }

    let mut eval_table = EvalTable::with_builtins(&mut symbol_table);
    let env_global = eval_table.env_global();
    for expr in parsed_exprs {
        let evaluated_expr = expr.eval(&mut eval_table, env_global);
        match evaluated_expr {
            Ok(expr) => println!("{}", expr.stringify(&symbol_table)),
            Err(err) => println!("{}", err.stringify(&symbol_table)),
        }
    }
}
