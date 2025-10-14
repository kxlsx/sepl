use std::collections::LinkedList;

use itertools::Itertools;
use strum_macros::{AsRefStr, Display, EnumIter};

use super::{expr_type_str, Env, EnvTable, Error, Expr, Lit, Procedure};

/// Type representing a bulitin procedure.
/// These are automatically assigned to their
/// symbol representations when creating
/// [`EnvTable::with_builtins`].
#[derive(EnumIter, AsRefStr, Display, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Builtin {
    /// Creates an anonymous procedure.
    #[strum(serialize = "lambda")]
    Lambda,
    /// Defines a symbol as an expression.
    #[strum(serialize = "define")]
    Define,
    /// Returns the argument without evaluating.
    #[strum(serialize = "quote")]
    Quote,
    /// Evaluates the argument and returns it.
    #[strum(serialize = "eval")]
    Eval,
    /// Evaluates all arguments and returns the
    /// last one.
    #[strum(serialize = "do")]
    Do,
    /// Return a list containing all args
    /// evaluated.
    #[strum(serialize = "list")]
    List,
    /// Returns the first item of a list.
    #[strum(serialize = "head")]
    Head,
    /// Returns the passed list without the first item.
    #[strum(serialize = "tail")]
    Tail,
    /// Returns the result of concatenating two lists.
    #[strum(serialize = "cat")]
    Concat,
    /// Evaluates and returns the second
    /// argument if the first is not `false`.
    /// otherwise evaluates and returns the third.
    #[strum(serialize = "if")]
    IfElse,
    /// Returns true if the arguments are
    /// equal.
    #[strum(serialize = "=")]
    Eq,
    /// Returns true if the first argument
    /// is less-than-or-equal to the second
    /// (both must be `float`s or `int`s).
    #[strum(serialize = "<=")]
    Leq,
    /// Adds numbers.
    #[strum(serialize = "+")]
    Add,
    /// Subtracts numbers.
    #[strum(serialize = "-")]
    Sub,
    /// Multiplies numbers.
    #[strum(serialize = "*")]
    Mul,
    /// Divides numbers.
    #[strum(serialize = "/")]
    Div,
    /// Performs integer division on numbers.
    #[strum(serialize = "//")]
    IntDiv,
    /// Returns the remainder of integer division.
    #[strum(serialize = "%")]
    Rem,
    /// Convert number to an `int``.
    #[strum(serialize = "int")]
    AsInt,
    /// Convert number to a `float`.
    #[strum(serialize = "float")]
    AsFloat,
}

impl Builtin {
    /// Evaluate the [`Builtin`] with the passed `args`.
    /// This method can be called as a result of of evaluating a
    /// [`List`](Expr::List) expression whose head is a [`Builtin`].
    ///
    /// # Errors
    /// Every builtin procedure accepts a specific
    /// number and type of arguments. Some builtins
    /// (like [`Do`](Builtin::Do))
    /// accept any non-zero number of arguments.
    ///
    /// [`eval`](Builtin::eval) can with [`Error::IncorrectArgCount`]
    /// and [`Error::IncorrectArgType`].
    pub fn eval(
        &self,
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        match self {
            Builtin::Lambda => Builtin::builtin_lambda(env_table, env, args),
            Builtin::Define => Builtin::builtin_define(env_table, env, args),
            Builtin::Quote => Builtin::builtin_quote(env_table, env, args),
            Builtin::Eval => Builtin::builtin_eval(env_table, env, args),
            Builtin::Do => Builtin::builtin_do(env_table, env, args),
            Builtin::List => Builtin::builtin_list(env_table, env, args),
            Builtin::Head => Builtin::builtin_head(env_table, env, args),
            Builtin::Tail => Builtin::builtin_tail(env_table, env, args),
            Builtin::Concat => Builtin::builtin_concat(env_table, env, args),
            Builtin::IfElse => Builtin::builtin_ifelse(env_table, env, args),
            Builtin::Eq => Builtin::builtin_eq(env_table, env, args),
            Builtin::Leq => Builtin::builtin_leq(env_table, env, args),
            Builtin::Add => Builtin::builtin_add(env_table, env, args),
            Builtin::Sub => Builtin::builtin_sub(env_table, env, args),
            Builtin::Mul => Builtin::builtin_mul(env_table, env, args),
            Builtin::Div => Builtin::builtin_div(env_table, env, args),
            Builtin::IntDiv => Builtin::builtin_intdiv(env_table, env, args),
            Builtin::Rem => Builtin::builtin_rem(env_table, env, args),
            Builtin::AsInt => Builtin::builtin_asint(env_table, env, args),
            Builtin::AsFloat => Builtin::builtin_asfloat(env_table, env, args),
        }
    }

    /// Symbolic value used to represent `ok`.
    fn builtin_ok(env_table: &mut EnvTable) -> Expr {
        Expr::Symbol(env_table.symbol_intern("ok"))
    }

    /// Symbolic value used to represent `nil`.
    fn builtin_nil(env_table: &mut EnvTable) -> Expr {
        Expr::Symbol(env_table.symbol_intern("nil"))
    }

    /// Symbolic value used to represent `true`.
    fn builtin_true(env_table: &mut EnvTable) -> Expr {
        Expr::Symbol(env_table.symbol_intern("true"))
    }

    /// Symbolic value used to represent `false`.
    fn builtin_false(env_table: &mut EnvTable) -> Expr {
        Expr::Symbol(env_table.symbol_intern("false"))
    }

    /// Check the 'truthiness' of the passed expression.
    fn builtin_boolean_check(expr: Expr, env_table: &mut EnvTable) -> bool {
        expr != Builtin::builtin_false(env_table) && expr != Builtin::builtin_nil(env_table)
    }

    /// Check whether two expressions match.
    /// 1. The expressions must be of the same type.
    /// 2. List equality is checked recursively, element by element.
    /// 3. Procedure equality is checked with reference equality
    /// 4. `int`s are coerced to `float`s if compared with a `float`.
    /// 5. Others are checked directly.
    fn builtin_expr_match(expr_a: Expr, expr_b: Expr) -> bool {
        match (expr_a, expr_b) {
            (Expr::List(list_a), Expr::List(list_b)) => {
                list_a.len() == list_b.len()
                    && list_a
                        .into_iter()
                        .zip(list_b)
                        .all(|(a, b)| Builtin::builtin_expr_match(a, b))
            }
            (Expr::Builtin(a), Expr::Builtin(b)) => a == b,
            (Expr::Procedure(a), Expr::Procedure(b)) => a == b,
            (Expr::Symbol(a), Expr::Symbol(b)) => a == b,
            (Expr::Lit(Lit::Float(a)), Expr::Lit(Lit::Float(b))) => a == b,
            (Expr::Lit(Lit::Float(a)), Expr::Lit(Lit::Int(b))) => a == b as f64,
            (Expr::Lit(Lit::Int(a)), Expr::Lit(Lit::Float(b))) => a as f64 == b,
            (Expr::Lit(Lit::Int(a)), Expr::Lit(Lit::Int(b))) => a == b,
            (_, _) => false,
        }
    }

    /// Creates an anonymous procedure.
    ///
    /// Accepts any non-zero number of arguments,
    /// all have to be `symbols`, except the last one
    /// which can be any expression.
    fn builtin_lambda(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Lambda),
                expected: 2,
                found: args.len(),
            });
        }

        let mut args_iter = args.into_iter();

        let params = match args_iter.next().unwrap().eval(env_table, env)? {
            Expr::List(list) => list
                .into_iter()
                .map(|e| match e {
                    Expr::Symbol(symbol) => Ok(symbol),
                    other_expr => Err(Error::IncorrectArgType {
                        builtin: Builtin::Lambda,
                        expected: expr_type_str!(Symbol),
                        found: other_expr.as_type_str(),
                    }),
                })
                .collect(),
            other_expr => Err(Error::IncorrectArgType {
                builtin: Builtin::Lambda,
                expected: expr_type_str!(List),
                found: other_expr.as_type_str(),
            }),
        }?;

        let body = args_iter.next().unwrap();

        Ok(Expr::Procedure(Procedure::new(
            params,
            Box::new(body),
            env,
            env_table,
        )))
    }

    /// Define a symbol as an expression.
    /// Returns `ok` as a `symbol`.
    ///
    /// Accepts 2 arguments; the first one
    /// is a `symbol`, the second one any
    /// expression.
    fn builtin_define(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Define),
                expected: 2,
                found: args.len(),
            });
        }

        let mut args_iter = args.into_iter();

        let symbol = match args_iter.next().unwrap().eval(env_table, env)? {
            Expr::Symbol(symbol) => Ok(symbol),
            other_expr => Err(Error::IncorrectArgType {
                builtin: Builtin::Define,
                expected: expr_type_str!(Symbol),
                found: other_expr.as_type_str(),
            }),
        }?;

        let body = args_iter.next().unwrap().eval(env_table, env)?;

        env_table.symbol_define(symbol, env, body);

        Ok(Builtin::builtin_ok(env_table))
    }

    /// Return the argument without evaluating.
    ///
    /// Accepts 1 argument.
    fn builtin_quote(
        _env_table: &mut EnvTable,
        _env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Quote),
                expected: 1,
                found: args.len(),
            });
        }

        Ok(args.into_iter().next().unwrap())
    }

    /// Evaluate the argument and return it.
    ///
    /// Accepts 1 argument.
    fn builtin_eval(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Eval),
                expected: 1,
                found: args.len(),
            });
        }

        args.into_iter()
            .next()
            .unwrap()
            .eval(env_table, env)?
            .eval(env_table, env)
    }

    /// Evaluate every argument and return the last one.
    ///
    /// Accepts any non-zero number of arguments.
    fn builtin_do(
        env_table: &mut EnvTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.is_empty() {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Do),
                expected: 1,
                found: 0,
            });
        }

        let arg_last = args.pop_back().unwrap();

        for arg in args {
            arg.eval(env_table, env)?;
        }

        arg_last.eval(env_table, env)
    }

    /// Return a list containing all args
    /// evaluated.
    fn builtin_list(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        let res: Result<_, Error> = args
            .into_iter()
            .map(|expr| expr.eval(env_table, env))
            .collect();
        Ok(Expr::List(res?))
    }

    /// Returns the first item of the passed list
    /// (or `nil` if it's empty).
    ///
    /// Accepts an argument that evaluates to a list.
    fn builtin_head(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Head),
                expected: 1,
                found: args.len(),
            });
        }

        match args.into_iter().next().unwrap().eval(env_table, env)? {
            Expr::List(mut list) => Ok(list.pop_front().unwrap_or(Builtin::builtin_nil(env_table))),
            other_expr => Err(Error::IncorrectArgType {
                builtin: Builtin::Head,
                expected: expr_type_str!(List),
                found: other_expr.as_type_str(),
            }),
        }
    }

    /// Returns the list without the first item.
    ///
    /// Accepts an argument that evaluates to a list.
    fn builtin_tail(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Tail),
                expected: 1,
                found: args.len(),
            });
        }

        match args.into_iter().next().unwrap().eval(env_table, env)? {
            Expr::List(mut list) => {
                list.pop_front();
                Ok(Expr::List(list))
            }
            other_expr => Err(Error::IncorrectArgType {
                builtin: Builtin::Tail,
                expected: expr_type_str!(List),
                found: other_expr.as_type_str(),
            }),
        }
    }

    /// Returns the result of concatenating two lists.
    ///
    /// Accepts two arguments that evaluate to two lists.
    fn builtin_concat(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Tail),
                expected: 1,
                found: args.len(),
            });
        }

        let mut args_iter = args.into_iter();

        let mut list_a = match args_iter.next().unwrap().eval(env_table, env)? {
            Expr::List(list) => Ok(list),
            other_expr => Err(Error::IncorrectArgType {
                builtin: Builtin::Concat,
                expected: expr_type_str!(List),
                found: other_expr.as_type_str(),
            }),
        }?;

        let mut list_b = match args_iter.next().unwrap().eval(env_table, env)? {
            Expr::List(list) => Ok(list),
            other_expr => Err(Error::IncorrectArgType {
                builtin: Builtin::Concat,
                expected: expr_type_str!(List),
                found: other_expr.as_type_str(),
            }),
        }?;

        list_a.append(&mut list_b);

        Ok(Expr::List(list_a))
    }

    /// Evaluates and returns the second
    /// argument if the first is not `false` or `nil`,
    /// otherwise evaluates and returns the third.
    ///
    /// Accepts 3 arguments.
    fn builtin_ifelse(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 3 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::IfElse),
                expected: 3,
                found: args.len(),
            });
        }

        let mut args_iter = args.into_iter();

        let cond = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)
            .map(|e| Ok(Builtin::builtin_boolean_check(e, env_table)))??;

        let exp1 = args_iter.next().unwrap();
        let exp2 = args_iter.next().unwrap();

        Ok(if cond {
            exp1.eval(env_table, env)?
        } else {
            exp2.eval(env_table, env)?
        })
    }

    /// Returns true if the arguments are equal.
    /// 1. The expressions must be of the same type.
    /// 2. List equality is checked recursively, element by element.
    /// 3. Procedure equality is checked with reference equality
    /// 4. Others are checked directly
    ///
    /// Accepts 2 arguments of any type.
    fn builtin_eq(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Eq),
                expected: 2,
                found: args.len(),
            });
        }

        let mut args_iter = args.into_iter();

        let expr_a = args_iter.next().unwrap().eval(env_table, env)?;
        let expr_b = args_iter.next().unwrap().eval(env_table, env)?;

        Ok(if Builtin::builtin_expr_match(expr_a, expr_b) {
            Builtin::builtin_true(env_table)
        } else {
            Builtin::builtin_false(env_table)
        })
    }

    /// Returns true if the first argument
    /// is less-than-or-equal to the second
    /// (both must be `float`s and/or `int`s).
    ///
    /// `int`s are coerced to `float`s if compared with a `float`.
    fn builtin_leq(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Leq),
                expected: 2,
                found: args.len(),
            });
        }

        let mut args_iter = args.into_iter();

        let a = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)
            .map(|e| match e {
                Expr::Lit(Lit::Float(num)) => Ok(Lit::Float(num)),
                Expr::Lit(Lit::Int(num)) => Ok(Lit::Int(num)),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Leq,
                    expected: expr_type_str!(numeric),
                    found: other_expr.as_type_str(),
                }),
            })??;
        let b = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)
            .map(|e| match e {
                Expr::Lit(Lit::Float(num)) => Ok(Lit::Float(num)),
                Expr::Lit(Lit::Int(num)) => Ok(Lit::Int(num)),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Leq,
                    expected: expr_type_str!(numeric),
                    found: other_expr.as_type_str(),
                }),
            })??;
        
        let cond = match (a, b) {
            (Lit::Float(a), Lit::Int(b)) => a <= b as f64,
            (Lit::Int(a), Lit::Float(b)) => a as f64 <= b,
            (Lit::Float(a), Lit::Float(b)) => a <= b,
            (Lit::Int(a), Lit::Int(b)) => a <= b,
        };

        Ok(if cond {
            Builtin::builtin_true(env_table)
        } else {
            Builtin::builtin_false(env_table)
        })
    }

    /// Return the result of 'folding'
    /// the passed arg list using the
    /// provided binary operation.
    ///
    /// There must be at least 2 args
    /// and every argument must be a
    /// `float` or `int`.
    fn builtin_binary_numeric<F: Fn(Lit, Lit) -> Lit>(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
        typ: Builtin,
        binary_operation: F,
    ) -> Result<Expr, Error> {
        if args.len() < 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(typ),
                expected: 2,
                found: args.len(),
            });
        }

        let res = args
            .into_iter()
            .map(|expr| match expr.eval(env_table, env) {
                Ok(Expr::Lit(Lit::Float(num))) => Ok(Lit::Float(num)),
                Ok(Expr::Lit(Lit::Int(num))) => Ok(Lit::Int(num)),
                Ok(other_expr) => Err(Error::IncorrectArgType {
                    builtin: typ,
                    expected: expr_type_str!(numeric),
                    found: other_expr.as_type_str(),
                }),
                Err(error) => Err(error),
            })
            .process_results(|nums| nums.reduce(binary_operation))?
            .unwrap();

        Ok(Expr::Lit(res))
    }

    /// Return the result of adding
    /// `floats` and/or `ints`.
    /// 
    /// Returns `int` only if all arguments are `ints`.
    fn builtin_add(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        Builtin::builtin_binary_numeric(
            env_table,
            env,
            args,
            Builtin::Add,
            |lit_a, lit_b| 
                match (lit_a, lit_b) {
                    (Lit::Float(a), Lit::Int(b)) => Lit::Float(a + b as f64),
                    (Lit::Int(a), Lit::Float(b)) => Lit::Float(a as f64 + b),
                    (Lit::Float(a), Lit::Float(b)) => Lit::Float(a + b),
                    (Lit::Int(a), Lit::Int(b)) => Lit::Int(a + b),
                }
        )
    }

    /// Return the result of subtracting
    /// `floats` and/or `ints`.    
    ///  
    /// Returns `int` only if all arguments are `ints`.
    fn builtin_sub(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        Builtin::builtin_binary_numeric(
            env_table,
            env,
            args,
            Builtin::Sub,
            |lit_a, lit_b| 
                match (lit_a, lit_b) {
                    (Lit::Float(a), Lit::Int(b)) => Lit::Float(a - b as f64),
                    (Lit::Int(a), Lit::Float(b)) => Lit::Float(a as f64 - b),
                    (Lit::Float(a), Lit::Float(b)) => Lit::Float(a - b),
                    (Lit::Int(a), Lit::Int(b)) => Lit::Int(a - b),
                }
        )
    }

    /// Return the result of multiplying
    /// `floats` and/or `ints`.
    /// 
    /// Returns `int` only if all arguments are `ints`.
    fn builtin_mul(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        Builtin::builtin_binary_numeric(
            env_table,
            env,
            args,
            Builtin::Mul,
            |lit_a, lit_b| 
                match (lit_a, lit_b) {
                    (Lit::Float(a), Lit::Int(b)) => Lit::Float(a * b as f64),
                    (Lit::Int(a), Lit::Float(b)) => Lit::Float(a as f64 * b),
                    (Lit::Float(a), Lit::Float(b)) => Lit::Float(a * b),
                    (Lit::Int(a), Lit::Int(b)) => Lit::Int(a * b),
                }
        )
    }

    /// Return the result of dividing
    /// `floats` and/or `ints`.
    /// 
    /// Always returns a `float`.
    fn builtin_div(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        Builtin::builtin_binary_numeric(
            env_table,
            env,
            args,
            Builtin::Div,
            |lit_a, lit_b| 
                match (lit_a, lit_b) {
                    (Lit::Float(a), Lit::Int(b)) => Lit::Float(a / b as f64),
                    (Lit::Int(a), Lit::Float(b)) => Lit::Float(a as f64 / b),
                    (Lit::Float(a), Lit::Float(b)) => Lit::Float(a / b),
                    (Lit::Int(a), Lit::Int(b)) => Lit::Float(a as f64 / b as f64),
                }
        )
    }

    /// Return the result of performing 
    /// integer division on `ints`.
    /// 
    /// Always returns an `int`.
    fn builtin_intdiv(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() < 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::IntDiv),
                expected: 2,
                found: args.len(),
            });
        }

        let res = args
            .into_iter()
            .map(|expr| match expr.eval(env_table, env) {
                Ok(Expr::Lit(Lit::Int(num))) => Ok(num),
                Ok(other_expr) => Err(Error::IncorrectArgType {
                    builtin: Builtin::IntDiv,
                    expected: expr_type_str!(Lit::Int),
                    found: other_expr.as_type_str(),
                }),
                Err(error) => Err(error),
            })
            .process_results(|mut nums| {
                let first = nums.next().unwrap();
                nums.try_fold(first, |acc, div| {
                    if div == 0 {
                        Err(Error::DivisionByZero { builtin: Builtin::IntDiv })
                    } else {
                        Ok(acc / div)
                    }
                })
            })??;

        Ok(Expr::Lit(Lit::Int(res)))
    }

    /// Return the remainder of performing 
    /// integer division on `ints`.
    /// 
    /// Always returns an `int`.
    fn builtin_rem(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() < 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Rem),
                expected: 2,
                found: args.len(),
            });
        }

        let res = args
            .into_iter()
            .map(|expr| match expr.eval(env_table, env) {
                Ok(Expr::Lit(Lit::Int(num))) => Ok(num),
                Ok(other_expr) => Err(Error::IncorrectArgType {
                    builtin: Builtin::Rem,
                    expected: expr_type_str!(Lit::Int),
                    found: other_expr.as_type_str(),
                }),
                Err(error) => Err(error),
            })
            .process_results(|mut nums| {
                let first = nums.next().unwrap();
                nums.try_fold(first, |acc, div| {
                    if div == 0 {
                        Err(Error::DivisionByZero { builtin: Builtin::Rem })
                    } else {
                        Ok(acc % div)
                    }
                })
            })??;

        Ok(Expr::Lit(Lit::Int(res)))
    }

    /// Convert the numeric argument to `int`.
    /// Does nothing to `int`s.
    ///
    /// Accepts 1 argument.
    fn builtin_asint(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::AsInt),
                expected: 1,
                found: args.len(),
            });
        }

        match args.into_iter().next().unwrap().eval(env_table, env)? {
            int @ Expr::Lit(Lit::Int(_)) => Ok(int),
            Expr::Lit(Lit::Float(float)) => Ok(Expr::Lit(Lit::Int(float as i64))),
            other_expr => Err(Error::IncorrectArgType {
                builtin: Builtin::AsInt,
                expected: expr_type_str!(numeric),
                found: other_expr.as_type_str(),
            }),
        }
    }

    /// Convert the numeric argument to `float`.
    /// Does nothing to `float`s.
    ///
    /// Accepts 1 argument.
    fn builtin_asfloat(
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::AsInt),
                expected: 1,
                found: args.len(),
            });
        }

        match args.into_iter().next().unwrap().eval(env_table, env)? {
            float @ Expr::Lit(Lit::Float(_)) => Ok(float),
            Expr::Lit(Lit::Int(int)) => Ok(Expr::Lit(Lit::Float(int as f64))),
            other_expr => Err(Error::IncorrectArgType {
                builtin: Builtin::AsInt,
                expected: expr_type_str!(numeric),
                found: other_expr.as_type_str(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    // TODO: builtin tests
}
