use std::collections::LinkedList;

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
    /// (both must be `float`s)
    #[strum(serialize = "<=")]
    Leq,
    /// Adds two `float`s
    #[strum(serialize = "+")]
    Add,
    /// Subtracts two `float`s
    #[strum(serialize = "-")]
    Sub,
    /// Multiplies two `float`s
    #[strum(serialize = "*")]
    Mul,
    /// Divides two `float`s
    #[strum(serialize = "/")]
    Div,
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
            Builtin::Lambda => self.builtin_lambda(env_table, env, args),
            Builtin::Define => self.builtin_define(env_table, env, args),
            Builtin::Quote => self.builtin_quote(env_table, env, args),
            Builtin::Eval => self.builtin_eval(env_table, env, args),
            Builtin::Do => self.builtin_do(env_table, env, args),
            Builtin::List => self.builtin_list(env_table, env, args),
            Builtin::Head => self.builtin_head(env_table, env, args),
            Builtin::Tail => self.builtin_tail(env_table, env, args),
            Builtin::Concat => self.builtin_concat(env_table, env, args),
            Builtin::IfElse => self.builtin_ifelse(env_table, env, args),
            Builtin::Eq => self.builtin_eq(env_table, env, args),
            Builtin::Leq => self.builtin_leq(env_table, env, args),
            Builtin::Add => self.builtin_add(env_table, env, args),
            Builtin::Sub => self.builtin_sub(env_table, env, args),
            Builtin::Mul => self.builtin_mul(env_table, env, args),
            Builtin::Div => self.builtin_div(env_table, env, args),
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
        expr != Builtin::builtin_false(env_table)
        && expr != Builtin::builtin_nil(env_table)
    }

    /// Check whether two expressions match.
    /// 1. The expressions must be of the same type.
    /// 2. List equality is checked recursively, element by element.
    /// 3. Procedure equality is checked with reference equality
    /// 4. Others are checked directly.
    fn builtin_expr_match(expr_a: Expr, expr_b: Expr) -> bool {
        match (expr_a, expr_b) {
            (Expr::List(list_a), Expr::List(list_b)) =>
                list_a.len() == list_b.len() 
                && list_a.into_iter().zip(list_b).all(|(a, b)| Builtin::builtin_expr_match(a, b)),
            (Expr::Builtin(a), Expr::Builtin(b)) => a == b,
            (Expr::Procedure(a), Expr::Procedure(b)) => a == b,
            (Expr::Symbol(a), Expr::Symbol(b)) => a == b,
            (Expr::Lit(Lit::Float(a)), Expr::Lit(Lit::Float(b))) => a == b,
            (_, _) => false,
        }
    }

    /// Creates an anonymous procedure.
    ///
    /// Accepts any non-zero number of arguments,
    /// all have to be `symbols`, except the last one
    /// which can be any expression.
    fn builtin_lambda(
        &self,
        env_table: &mut EnvTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Lambda),
                expected: 2,
                found: args.len(),
            });
        }

        let params = args
            .pop_front()
            .map(|e| match e.eval(env_table, env)? {
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
            })
            .unwrap()?;

        let body = args.pop_front().unwrap();

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
        &self,
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

        let symbol = args_iter
            .next()
            .map(|e| match e.eval(env_table, env)? {
                Expr::Symbol(symbol) => Ok(symbol),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Define,
                    expected: expr_type_str!(Symbol),
                    found: other_expr.as_type_str(),
                }),
            })
            .unwrap()?;
        let body = args_iter.next().unwrap().eval(env_table, env)?;

        env_table.symbol_define(symbol, env, body);

        Ok(Builtin::builtin_ok(env_table))
    }

    /// Return the argument without evaluating.
    ///
    /// Accepts 1 argument.
    fn builtin_quote(
        &self,
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
        &self,
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
        &self,
        env_table: &mut EnvTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() < 1 {
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
        &self,
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        let res: Result<_, Error> = args.into_iter().map(|expr| expr.eval(env_table, env)).collect();
        Ok(Expr::List(res?))
    }

    /// Returns the first item of the passed list
    /// (or `nil` if it's empty).
    ///
    /// Accepts an argument that evaluates to a list.
    fn builtin_head(
        &self,
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

        args.into_iter()
            .next()
            .map(|e| match e.eval(env_table, env)? {
                Expr::List(mut list) => Ok(list.pop_front().unwrap_or(Builtin::builtin_nil(env_table))),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Head,
                    expected: expr_type_str!(List),
                    found: other_expr.as_type_str(),
                }),
            })
            .unwrap()
    }

    /// Returns the list without the first item.
    ///
    /// Accepts an argument that evaluates to a list.
    fn builtin_tail(
        &self,
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

        args.into_iter()
            .next()
            .map(|e| match e.eval(env_table, env)? {
                Expr::List(mut list) => {
                    list.pop_front();
                    Ok(Expr::List(list))
                }
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Tail,
                    expected: expr_type_str!(List),
                    found: other_expr.as_type_str(),
                }),
            })
            .unwrap()
    }

    /// Returns the result of concatenating two lists.
    ///
    /// Accepts two arguments that evaluate to two lists.
    fn builtin_concat(
        &self,
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

        let mut a = args_iter
            .next()
            .map(|e| match e.eval(env_table, env)? {
                Expr::List(list) => Ok(list),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Concat,
                    expected: expr_type_str!(List),
                    found: other_expr.as_type_str(),
                }),
            })
            .unwrap()?;

        let mut b = args_iter
            .next()
            .map(|e| match e.eval(env_table, env)? {
                Expr::List(list) => Ok(list),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Concat,
                    expected: expr_type_str!(List),
                    found: other_expr.as_type_str(),
                }),
            })
            .unwrap()?;

        a.append(&mut b);

        Ok(Expr::List(a))
    }

    /// Evaluates and returns the second
    /// argument if the first is not `false` or `nil`,
    /// otherwise evaluates and returns the third.
    ///
    /// Accepts 3 arguments.
    fn builtin_ifelse(
        &self,
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

        let cond = args_iter.next().unwrap().eval(env_table, env).map(|e| {
            Ok(Builtin::builtin_boolean_check(e, env_table))
        })??;

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
        &self,
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

        let expr_a = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)?;
        let expr_b = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)?;
        
        Ok(
            if Builtin::builtin_expr_match(expr_a, expr_b) {
                Builtin::builtin_true(env_table)
            } else {
                Builtin::builtin_false(env_table)
            }
        )
    }

    /// Returns true if the first argument
    /// is less-than-or-equal to the second
    /// (both must be `float`s).
    ///
    /// Accepts 2 `float` arguments.
    fn builtin_leq(
        &self,
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
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Leq,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;
        let b = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)
            .map(|e| match e {
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Leq,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;
            
        Ok(
            if a <= b {
                Builtin::builtin_true(env_table)
            } else {
                Builtin::builtin_false(env_table)
            }
        )
    }

    /// Return the result of adding
    /// two `float`s
    fn builtin_add(
        &self,
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Add),
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
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Add,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;
        let b = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)
            .map(|e| match e {
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Add,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;

        Ok(Expr::Lit(Lit::Float(a + b)))
    }

    /// Return the result of subtracting
    /// two `float`s
    fn builtin_sub(
        &self,
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Sub),
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
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Sub,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;
        let b = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)
            .map(|e| match e {
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Sub,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;

        Ok(Expr::Lit(Lit::Float(a - b)))
    }

    /// Return the result of multiplying
    /// two `float`s
    fn builtin_mul(
        &self,
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Mul),
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
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Mul,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;
        let b = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)
            .map(|e| match e {
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Mul,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;

        Ok(Expr::Lit(Lit::Float(a * b)))
    }

    /// Return the result of dividing
    /// two `float`s
    fn builtin_div(
        &self,
        env_table: &mut EnvTable,
        env: Env,
        args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Div),
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
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Div,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;
        let b = args_iter
            .next()
            .unwrap()
            .eval(env_table, env)
            .map(|e| match e {
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Div,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;

        Ok(Expr::Lit(Lit::Float(a / b)))
    }
}

#[cfg(test)]
mod tests {
    // TODO: builtin tests
}
