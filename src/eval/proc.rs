use std::collections::LinkedList;
use std::fmt;

use super::{Env, EnvTable, Error, Expr, Lit, Result, Symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Procedure<'i> {
    params: LinkedList<Symbol<'i>>,
    body: Box<Expr<'i>>,
    created_in_env: Env, // TODO: quick fix, check this out later.
}

impl<'i> Procedure<'i> {
    pub fn eval(
        self,
        env_table: &mut EnvTable<'i>,
        parent_env: Env,
        args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if self.params.len() != args.len() {
            return Err(Error::IncorrectArgCount);
        }

        // FIXME: test whether this works; i'm currently
        // creating the new env as a child of the procedure's original env,
        // but evaluating the arguments in the current parent env.
        let env = env_table.create_env(self.created_in_env);

        for (param, arg) in self.params.iter().zip(args) {
            let arg_eval = arg.eval(env_table, parent_env)?;
            env_table.define_symbol(*param, env, arg_eval);
        }

        // println!("{:?}", env_table.resolve_symbol(Symbol::from("n"), env));
        // println!("{:?}", env_table.resolve_symbol(Symbol::from("fib"), env));
        //println!("{:?}", (self.parent_env, _parent_env));
        // for ((a, e), _) in &env_table.symbol_table {
        //     if e.0 != 0 && a. {
        //         println!("{:?} {:?}", a, e);
        //     }
            
        // }

        // FIXME: gotta clean up unneeded envs somehow
        let res = self.body.eval(env_table, env);
        // for param in self.params {
        //     env_table.undefine_symbol(param, env);
        // }

        res
    }
}

impl fmt::Display for Procedure<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}", Builtin::Lambda)?;

        for symbol in &self.params {
            write!(f, " {}", symbol)?;
        }

        write!(f, " {})", self.body)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Builtin {
    Lambda,
    Define,
    Quote,
    Eval,
    Print,
    IfElse,
    Leq,
    Add,
    Sub,
    Mul,
    Div,
}

impl Builtin {
    pub fn eval<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        match self {
            Builtin::Lambda => self.builtin_lambda(env_table, env, args),
            Builtin::Define => self.builtin_define(env_table, env, args),
            Builtin::Quote => self.builtin_quote(env_table, env, args),
            Builtin::Eval => self.builtin_eval(env_table, env, args),
            Builtin::Print => self.builtin_print(env_table, env, args),
            Builtin::IfElse => self.builtin_ifelse(env_table, env, args),
            Builtin::Leq => self.builtin_leq(env_table, env, args),
            Builtin::Add => self.builtin_add(env_table, env, args),
            Builtin::Sub => self.builtin_sub(env_table, env, args),
            Builtin::Mul => self.builtin_mul(env_table, env, args),
            Builtin::Div => self.builtin_div(env_table, env, args),
        }
    }

    fn builtin_lambda<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        let body = args.pop_back().ok_or(Error::IncorrectArgCount)?;

        let params = args
            .into_iter()
            .map(|e| {
                if let Expr::Symbol(symbol) = e.eval(env_table, env)? {
                    Ok(symbol)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })
            .collect::<Result<LinkedList<Symbol>>>()?;

        let proc = Expr::Procedure(Procedure {
            params,
            body: Box::new(body),
            created_in_env: env,
        });

        Ok(proc)
    }

    fn builtin_define<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount)
        }

        let symbol = args.pop_front().ok_or(Error::IncorrectArgCount).map(|e| {
            if let Expr::Symbol(symbol) = e.eval(env_table, env)? {
                Ok(symbol)
            } else {
                Err(Error::IncorrectArgType)
            }
        })??;
        let body = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(env_table, env)?;

        env_table.define_symbol(symbol, env, body);

        Ok(Expr::Lit(Lit::Nil)) // TODO: ???
    }

    fn builtin_quote<'i>(
        &self,
        _env_table: &mut EnvTable<'i>,
        _env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount)
        }

        Ok(args.pop_front().unwrap())
    }

    fn builtin_eval<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 1 {
            return Err(Error::IncorrectArgCount);
        }

        args
        .pop_front()
        .unwrap()
        .eval(env_table, env)?.eval(env_table, env)
    }

    fn builtin_print<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() < 1 {
            return Err(Error::IncorrectArgCount)
        }

        for arg in args {
            print!("{} ", arg.eval(env_table, env)?);
        }

        Ok(Expr::Lit(Lit::Nil))
    }

    fn builtin_ifelse<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 3 {
            return Err(Error::IncorrectArgCount)
        }

        let cond = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(env_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Bool(cond)) = e {
                    Ok(cond)
                } else if let Expr::Lit(Lit::Nil) = e { //FIXME: temporary, maybe add 'ifnull'?
                    Ok(false)
                } else {
                    Ok(true)
                }
            })??;

        let exp1 = args.pop_front().ok_or(Error::IncorrectArgCount)?;
        let exp2 = args.pop_front().ok_or(Error::IncorrectArgCount)?;

        Ok(if cond {
            exp1.eval(env_table, env)?
        } else {
            exp2.eval(env_table, env)?
        })
    }

    fn builtin_leq<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount)
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(env_table, env)
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
            .eval(env_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Bool(a <= b)))
    }

    fn builtin_add<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount)
        }
        
        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(env_table, env)
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
            .eval(env_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Float(a + b)))
    }

    fn builtin_sub<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount)
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(env_table, env)
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
            .eval(env_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Float(a - b)))
    }

    fn builtin_mul<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount)
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(env_table, env)
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
            .eval(env_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Float(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
                }
            })??;

        Ok(Expr::Lit(Lit::Float(a * b)))
    }

    fn builtin_div<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        if args.len() != 2 {
            return Err(Error::IncorrectArgCount)
        }

        let a = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(env_table, env)
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
            .eval(env_table, env)
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

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Builtin::Lambda => "lambda",
            Builtin::Define => "define",
            Builtin::Quote => "quote",
            Builtin::Eval => "eval",
            Builtin::Print => "print",
            Builtin::IfElse => "ifelse",
            Builtin::Leq => "<=",
            Builtin::Add => "+",
            Builtin::Sub => "-",
            Builtin::Mul => "*",
            Builtin::Div => "/",
        }
        .fmt(f)
    }
}
