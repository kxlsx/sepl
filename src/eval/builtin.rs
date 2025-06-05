use strum_macros::{AsRefStr, Display, EnumIter};

use super::{expr_type_str, Env, Error, EvalTable, Expr, Lit, Procedure, Symbol};

#[derive(EnumIter, AsRefStr, Display, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Builtin {
    #[strum(serialize = "lambda")]
    Lambda,
    #[strum(serialize = "define")]
    Define,
    #[strum(serialize = "quote")]
    Quote,
    #[strum(serialize = "eval")]
    Eval,
    #[strum(serialize = "do")]
    Do,
    #[strum(serialize = "if")]
    IfElse,
    #[strum(serialize = "<=")]
    Leq,
    #[strum(serialize = "+")]
    Add,
    #[strum(serialize = "-")]
    Sub,
    #[strum(serialize = "*")]
    Mul,
    #[strum(serialize = "/")]
    Div,
}

impl Builtin {
    pub fn eval(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
    ) -> Result<Expr, Error> {
        match self {
            Builtin::Lambda => self.builtin_lambda(eval_table, env, args),
            Builtin::Define => self.builtin_define(eval_table, env, args),
            Builtin::Quote => self.builtin_quote(eval_table, env, args),
            Builtin::Eval => self.builtin_eval(eval_table, env, args),
            Builtin::Do => self.builtin_do(eval_table, env, args),
            Builtin::IfElse => self.builtin_ifelse(eval_table, env, args),
            Builtin::Leq => self.builtin_leq(eval_table, env, args),
            Builtin::Add => self.builtin_add(eval_table, env, args),
            Builtin::Sub => self.builtin_sub(eval_table, env, args),
            Builtin::Mul => self.builtin_mul(eval_table, env, args),
            Builtin::Div => self.builtin_div(eval_table, env, args),
        }
    }

    fn builtin_lambda(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: Vec<Expr>,
    ) -> Result<Expr, Error> {
        let body = args.pop().ok_or(Error::IncorrectArgCount {
            expr: Expr::Builtin(Builtin::Lambda),
            expected: 1,
            found: 0,
        })?;

        let params = args
            .into_iter()
            .map(|e| match e.eval(eval_table, env)? {
                Expr::Symbol(symbol) => Ok(symbol),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Lambda,
                    expected: expr_type_str!(Symbol),
                    found: other_expr.as_type_str(),
                }),
            })
            .collect::<Result<Vec<Symbol>, Error>>()?;

        Ok(Expr::Procedure(Procedure::new(
            params,
            Box::new(body),
            env,
            eval_table,
        )))
    }

    fn builtin_define(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
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
            .map(|e| match e.eval(eval_table, env)? {
                Expr::Symbol(symbol) => Ok(symbol),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Define,
                    expected: expr_type_str!(Symbol),
                    found: other_expr.as_type_str(),
                }),
            })
            .unwrap()?;
        let body = args_iter.next().unwrap().eval(eval_table, env)?;

        // This prevents recursive definitions
        // e.g. '(define x x)'
        match body {
            Expr::Symbol(body_symbol) if body_symbol == symbol => (),
            _ => {
                eval_table.symbol_define(symbol, env, body);
            }
        }

        Ok(Expr::Lit(Lit::Nil))
    }

    fn builtin_quote(
        &self,
        _eval_table: &mut EvalTable,
        _env: Env,
        args: Vec<Expr>,
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

    fn builtin_eval(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
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
            .eval(eval_table, env)?
            .eval(eval_table, env)
    }

    fn builtin_do(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: Vec<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() <= 1 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::Do),
                expected: 1,
                found: 0,
            });
        }

        let arg_last = args.pop().unwrap();

        for arg in args {
            arg.eval(eval_table, env)?;
        }

        arg_last.eval(eval_table, env)
    }

    fn builtin_ifelse(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 3 {
            return Err(Error::IncorrectArgCount {
                expr: Expr::Builtin(Builtin::IfElse),
                expected: 3,
                found: args.len(),
            });
        }

        let mut args_iter = args.into_iter();

        let cond = args_iter.next().unwrap().eval(eval_table, env).map(|e| {
            if let Expr::Lit(Lit::Bool(cond)) = e {
                Ok(cond)
            } else if let Expr::Lit(Lit::Nil) = e {
                //FIXME: temporary, maybe add 'ifnull'?
                Ok(false)
            } else {
                Ok(true)
            }
        })??;

        let exp1 = args_iter.next().unwrap();
        let exp2 = args_iter.next().unwrap();

        Ok(if cond {
            exp1.eval(eval_table, env)?
        } else {
            exp2.eval(eval_table, env)?
        })
    }

    fn builtin_leq(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
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
            .eval(eval_table, env)
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
            .eval(eval_table, env)
            .map(|e| match e {
                Expr::Lit(Lit::Float(cond)) => Ok(cond),
                other_expr => Err(Error::IncorrectArgType {
                    builtin: Builtin::Leq,
                    expected: expr_type_str!(Lit::Float),
                    found: other_expr.as_type_str(),
                }),
            })??;

        Ok(Expr::Lit(Lit::Bool(a <= b)))
    }

    fn builtin_add(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
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
            .eval(eval_table, env)
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
            .eval(eval_table, env)
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

    fn builtin_sub(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
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
            .eval(eval_table, env)
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
            .eval(eval_table, env)
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

    fn builtin_mul(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
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
            .eval(eval_table, env)
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
            .eval(eval_table, env)
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

    fn builtin_div(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        args: Vec<Expr>,
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
            .eval(eval_table, env)
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
            .eval(eval_table, env)
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
