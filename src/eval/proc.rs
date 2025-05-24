use std::collections::LinkedList;
use std::fmt;

use super::{Env, EnvTable, Error, Expr, Lit, Result, Symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Procedure<'i> {
    params: LinkedList<Symbol<'i>>,
    body: Box<Expr<'i>>,
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

        let env = env_table.create_env(parent_env);

        for (param, arg) in self.params.iter().zip(args) {
            let arg_eval = arg.eval(env_table, parent_env)?;
            env_table.define_symbol(*param, env, arg_eval);
        }

        let res = self.body.eval(env_table, env);
        for param in self.params {
            env_table.undefine_symbol(param, env);
        }

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
            Builtin::Lambda => self.lambda(env_table, env, args),
            Builtin::Define => self.define(env_table, env, args),
            Builtin::IfElse => self.ifelse(env_table, env, args),
            Builtin::Leq => self.leq(env_table, env, args),
            Builtin::Add => self.add(env_table, env, args),
            Builtin::Sub => self.sub(env_table, env, args),
            Builtin::Mul => self.mul(env_table, env, args),
            Builtin::Div => self.div(env_table, env, args),
        }
    }

    fn lambda<'i>(
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
        });

        Ok(proc)
    }

    pub fn define<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
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

    pub fn ifelse<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
        let cond = args
            .pop_front()
            .ok_or(Error::IncorrectArgCount)?
            .eval(env_table, env)
            .map(|e| {
                if let Expr::Lit(Lit::Bool(cond)) = e {
                    Ok(cond)
                } else {
                    Err(Error::IncorrectArgType)
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

    pub fn leq<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
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

    pub fn add<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
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

    pub fn sub<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
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

    pub fn mul<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
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

    pub fn div<'i>(
        &self,
        env_table: &mut EnvTable<'i>,
        env: Env,
        mut args: LinkedList<Expr<'i>>,
    ) -> Result<Expr<'i>> {
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
