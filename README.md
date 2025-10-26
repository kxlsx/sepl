<!-- TODO: UPDATE README:
    1. ((lambda (x) x) x) no longer crashes
    2. Symbols definitions are evaluated on calling define,
       so this produces following behaviour:
        > (define x y) (define y z) (define z x)
        nil nil nil
        > x y z
        y z y
       (look up the name of this resolving scheme in Structure & Interpretation of Computer Programs)
-->
# <img src="./res/img/mouth.gif" height="20"> sepl <img src="./res/img/mouth.gif" height="20">

An interpreter for a minimal [`lisp`](https://en.wikipedia.org/wiki/Lisp_(programming_language)).

This is a project that I made 90% for fun and 10% for a grade.
I probably won't ever implement all of the features and optimizations
I'd want, (as I unfortunately don't yet posess infinite time), but
most of the basic `lisp` special forms are here.

## Building

The project is split into two rust crates:
- `sepl-lib` - containing the parser & evaluation engine.
- `sepl` - containing a command line REPL. 

```
$ cargo build     # compile both the library and the binary
$ cargo test      # run unit tests
$ cargo run       # compile and run the binary
```

## Usage

The binary package is an interactive REPL shell.
Every typed `sepl` expression is first evaluated and then
printed to standard output.

Tokens preceded by a `:` are treated as special 
interpreter commands.

### Interpreter commands

- `:q`, `:quit`

    Exit the shell.
- `:h`, `:help`

    Print all interpreter commands.
- `:d`, `:defined`

    Print every symbol defined in the global scope.

## Syntax

Every sepl expression can be written as:
```
<EXPR> ::= ( <LIST> ) | [ <LIST> ] | { <LIST> } | <LITERAL> | <SYMBOL>
<LIST> ::= <EXPR> <LIST> | ɛ
```
where `<LITERAL>` can be any literal
associated with a type and
`<SYMBOL>` can be any string of unicode letters, punctuation or digits 
that's not a literal.

### Types

There are currently 7 basic types recognized by the `sepl` language:

| Name | Description |
| --- | --- |
| `list` | linked list of expressions. |
| `procedure` | anonymous procedure defined with the lambda `builtin`. |
| `builtin` | special form built into the language (`+`, `-`, `quote`, `lambda`, etc.). |
| `float` | double precision floating point number. |
| `int`   | 64-bit integer. |
| `symbol` | value representing a unique string of characters. |

#### Floats

Floating point numbers must be in a form
recognized by the `rust` programming language i. e.
a number with a decimal point or in scientific notation.

#### Symbols

Every symbol represents a unique string of characters,
so two identical strings are treated as the same symbol.

### Evaluation

There are 3 evaluation rules:

1. `procedures`, `bultins`, `floats`, `ints`
always yield themselves.
2. `symbols` evaluate to their definitions in the current scope, if the `symbol` contains no definition
it yields itself.
3. If the head of a `list` evaluates to a `builtin` or
`procedure` it is treated as a function call
with the `list's` tail serving as arguments, otherwise
the `list` evaluates every item and yields itself.
4. When called, `procedures` always recursively
evaluate their arguments, `builtins` make no such
promise.

#### Examples:

**1: First rule**
```lisp
1.5 true false nil + 32
```
*evaluates to:*
```lisp
1.5 true false nil + 32
```

**2: Second Rule**
```lisp
kotek Kotek Piesek
```
*with kotek defined as 3.14, evaluates to:*
```lisp
3.14 Kotek Piesek
```

**3: Third Rule**
```lisp
(+ 1.5 1.5) (true false 2.0) ((* 2 2) bum nil)
```
*evaluates to:*
```lisp
3.0 (true false 2.0) (4 bum nil)
```

**4: Fourth Rule**
```lisp
((lambda (x y) (+ x y)) 1.5 (+ 1 1)) (quote (+ 1.5 2.0))
```
*evaluates to:*
```lisp
3.5 (+ 1.5 2.0)
```

### Special Forms

Special forms are represented by the `builtin` type and
represent basic operations built into the language. 
Every `builtin` is by default associated with a `symbol`
defined in the global scope.  

The list of recognized `builtins` is as follows: 

| Default symbol | Arguments | Behaviour | Yields |
| --- | --- | --- | --- |
| lambda | A `list` of `symbols` & any expression | Evaluates only the first argument, treats the second one as the `procedure`'s 'body'. | An anonymous `procedure` accepting arguments represented by symbols in the passed list. The created `procedure` always captures the current scope. |
| define | A `symbol` & any expression | Evaluates both arguments, the first one must yield a `symbol`. Defines the passed `symbol` with the result of evaluating the second argument. | The `ok` `symbol`. |
| quote | Any expression | Does not evaluate the argument. | The passed argument, without evaluating it. |
| eval | Any expression | Evaluates the argument. | The result of evaluating the argument. |
| do | Any number of expressions | Evaluates every argument. | Evaluates every argument and returns the result of the last one. |
| list | Any number of expressions. | Evaluates every argument | A `list` containing every evaluated argument. |
| head | A `list` | Evaluates the argument. | The first item of the `list` or `nil` if it's empty |
| tail | A `list` | Evaluates the argument. | The passed `list` without the first item. |
| cat | Two `lists` | Evaluates both arguments | The two passed `lists` concatenated into one. |
| if | Three expressions | Evaluates the first argument.  Only the second argument is evaluated if and only if the first argument is not `false` or `nil`, otherwise only the third argument is evaluated. | The result of the first passed expression if the first argument is not `false` or `nil`, otherwise the result of the second passed expression. |
| =  | Two expressions     | Evaluates both arguments. | `true` if the arguments are equal, `false` otherwise. Equal expressions must be of the same type. `procedures` are equal if and only if they are the same instance of the `procedure`. `ints` are coerced to `floats` if compared to other `floats`. `lists` are checked item by item, recursively. |
| <= | Two `floats\|ints`  | Evaluates both arguments | `true` if the first argument is less than or equal to the second one, otherwise `false`. |
| +, -, *, / | At least two `floats\|ints` | Evaluates all arguments. | The result of the associated arithmetic operation. |
| // | At least two `ints` | Evaluates all arguments. | The result of integer division. |
| % | At least two `ints` | Evaluates all arguments. | The result of the [modulo](https://en.wikipedia.org/wiki/Modulo) operation. |
| float, int | One `float\|int` | Evaluates the argument. | Convert the argument to `float` and `int` respectively. |

### Scope

Every defined `symbol` exists in a scope. The default one is
called the global scope. Every created `procedure` captures
the environment it's been created in and creates a scope of its
own when called. When searching for a `symbol` definition, the interpreter searches recursively, starting in the current scope
and going up the captured scope tree, eventually ending at the global scope.

`symbol` definitions are automatically garbage-collected when
they are no longer accessible.

## Examples

The factorial function:
```lisp
(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
```

Two Fibonacci procedures:
```lisp
(define fib (lambda (a b n) (if (= n 0) a (fib b (+ a b) (- n 1)))))

(define fib_exp (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib_exp (- n 1)) (fib_exp (- n 2)))))))
```

[cons](https://en.wikipedia.org/wiki/Cons), [car and cdr](https://en.wikipedia.org/wiki/CAR_and_CDR) defined as lambdas:
```lisp
(define cons (lambda (x y) (lambda (m) (m x y))))
(define car (lambda (z) (z (lambda (p q) p))))
(define cdr (lambda (z) (z (lambda (p q) q))))
```

## Caveats

The evaluation engine relies on recursion,
so it's quite easy to overflow the stack while
creating heavily recursive functions.
Stack overflows are not currently handled gracefully
and they just crash the binary.

There's also quite a bit more special forms one could 
implement (like `defmacro`), but pretty much everything
can be done with the current set.

There's a fair chance that I'm never going to fix
any of these issues, but who knows.

## Links

Mouth gif from: [animated_images.org](https://www.animatedimages.org/img-animated-mouth-image-0014-65499.htm)