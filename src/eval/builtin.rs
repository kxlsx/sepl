use std::collections::LinkedList;

use strum_macros::{Display, EnumIter, AsRefStr};

use super::{Env, Error, EvalTable, Expr, Lit, Procedure, Symbol};

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
        args: LinkedList<Expr>,
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
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        let body = args.pop_back().ok_or(Error::IncorrectArgCount)?;

        let params = args
            .into_iter()
            .map(|e| {
                if let Expr::Symbol(symbol) = e.eval(eval_table, env)? {
                    Ok(symbol)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })
            .collect::<Result<LinkedList<Symbol>, Error>>()?;

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
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount);
        }

        let symbol = args.pop_front().ok_or(Error::IncorrectArgCount).map(|e| {
            if let Expr::Symbol(symbol) = e.eval(eval_table, env)? {
                Ok(symbol)
            } else {
                Err(Error::IncorrectArgType)
            }
        })??;
        let body = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)?;

        eval_table.symbol_define(symbol, env, body);

        Ok(Expr::Lit(Lit::Nil)) // TODO: ???
    }

    fn builtin_quote(
        &self,
        _eval_table: &mut EvalTable,
        _env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount);
        }

        Ok(args.pop_front().unwrap())
    }

    fn builtin_eval(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount);
        }

        args.pop_front()
            .unwrap()
            .eval(eval_table, env)?
            .eval(eval_table, env)
    }

    fn builtin_do(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() <= 1 {
            return Err(Error::IncorrectArgCount);
        }

        let arg_last = args.pop_back().unwrap();

        for arg in args {
            arg.eval(eval_table, env)?;
        }

        arg_last.eval(eval_table, env)
    }

    fn builtin_ifelse(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 3 {
            return Err(Error::IncorrectArgCount);
        }

        let cond = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Bool(cond)) = e {
                    Ok(cond)
                } else if let Expr::Lit(Lit::Nil) = e {
                    //FIXME: temporary, maybe add 'ifnull'?
                    Ok(false)
                } else {
                    Ok(true)
                }
            })??;

        let exp1 = args.pop_front().ok_or(Error::IncorrectArgCount)?;
        let exp2 = args.pop_front().ok_or(Error::IncorrectArgCount)?;

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
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount);
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;
        let b = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Bool(a <= b)))
    }

    fn builtin_add(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount);
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;
        let b = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Float(a + b)))
    }

    fn builtin_sub(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount);
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;
        let b = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Float(a - b)))
    }

    fn builtin_mul(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount);
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;
        let b = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Float(a * b)))
    }

    fn builtin_div(
        &self,
        eval_table: &mut EvalTable,
        env: Env,
        mut args: LinkedList<Expr>,
    ) -> Result<Expr, Error> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount);
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;
        let b = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(eval_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Float(a / b)))
    }
}

