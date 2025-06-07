# sepl

Interpreter dialektu [lispa](https://en.wikipedia.org/wiki/Lisp_(programming_language)).

## Kompilacja i uruchomienie

Program kompiluje się za pomocą narzędzia `cargo` za pomocą komendy:
```
$ cargo build
```
Wbudowane testy można wykonać za pomocą komendy:
```
$ cargo test
```
Powłokę interpretera wykonuje się za pomocą
komendy
```
$ cargo run
```

## Struktura projektu

Projekt podzielony jest na część biblioteczną `sepl_lib/`
oraz na część wykonywalną: `sepl_bin`.

Zawarta biblioteka zawiera całą funkcjonalność interpretera
opisaną w dokumentacji dostępnej po wykonaniu komendy:
```
$ cargo doc
```

Plik wykonywalny jest natomiast prostą powłoką typu [`REPL`](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
pozwalającą na wykonywanie wyrażeń języka.

## Opis składni

Język `sepl` jest dialektem [lispa](https://en.wikipedia.org/wiki/Lisp_(programming_language)) zatem w całości składa się z tzw. `wyrażeń`.
Wyrażeniami są:

- Literały - Liczby zmiennoprzecinkowe, `true`, `false` oraz `nil`.
- Symbole - Łańcuchy liter, cyfr i znaków specjalnych, które nie są
literałami.
- Wywołania procedur - Konstrukcje typu: '(' \<lista_wyrażeń\> ')'.

W języku zawarte są następujące wbudowane procedury:
- `(define x expr)` - Definiuje symbol `x` jako wyrażenie `expr`.
- `(lambda x1 ... xn expr)` - Tworzy anonimową procedurę 
o argumentach `x1, ... xn` i ciele `expr`
- `(quote expr)` - Zwraca `expr` bez ewaluacji.
- `(eval expr)` - Ewaluuje i zwraca `expr`.
- `(do expr1 ... expr_n)` - Ewaluuje wszystkie wyrażenia i zwraca ostatnie.
- `(if cond expr_a expr_b)` - Zwraca wynik `expr_a` jeżeli `cond` jest `true` albo nie jest `nil`, inaczej zwraca wynik `expr_b`.
- `(<= a b)` - Zwraca czy liczba `a` jest mniejsza lub równa `b`.
- `+`, `-`, `*`, `/` - Dokonują operacji matematycznych na liczbach.

## Przykładowe programy
Program liczący wynik 52!:
```lisp
(define = (lambda a b (if (<= a b) (<= b a) false)))
(define fact (lambda n (if (= n 0.0) 1. (* n (fact (- n 1.))))))
(fact 52.0)
```
Program liczący sumę listy zawierającej
liczby 1, 2, 3.
```lisp
(define cons (lambda x y (lambda m (m x y))))
(define car (lambda z (z (lambda p q p))))
(define cdr (lambda z (z (lambda p q q))))
(define list (cons 1.0 (cons 2. (cons 3. nil))))
(define sum (lambda xs (if (cdr xs) (+ (car xs) (sum (cxs))) (car xs))))
(sum list)
```

## Problemy
Język w obecnym stanie nie zawiera liczb całkowitych. 
Z powodu limitowanego czasu zaimplementowane zostały
tylko liczby zmiennoprzecinkowe.
