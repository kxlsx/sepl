use super::{Env, Error, EnvTable, Expr, Symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Procedure {
    params: Vec<Symbol>,
    body: Box<Expr>,
    capture_env: Env,
}

impl Procedure {
    pub fn new(
        params: Vec<Symbol>,
        body: Box<Expr>,
        parent_env: Env,
        env_table: &mut EnvTable,
    ) -> Self {
        Self {
            params,
            body,
            capture_env: env_table.env_create(parent_env),
        }
    }

    pub fn eval(
        self,
        env_table: &mut EnvTable,
        parent_env: Env,
        args: Vec<Expr>,
    ) -> Result<Expr, Error> {
        if self.params.len() != args.len() {
            return Err(Error::IncorrectArgCount {
                expected: self.params.len(),
                found: args.len(),
                expr: Expr::Procedure(self),
            });
        }

        let env = env_table.env_create(self.capture_env);

        for (param, arg) in self.params.iter().zip(args) {
            let arg_eval = arg.eval(env_table, parent_env)?;
            env_table.symbol_define(*param, env, arg_eval);
        }

        let res = self.body.eval(env_table, env);

        env_table.env_try_destroy(env);

        res
    }

    pub fn captured_env(&self) -> Env {
        self.capture_env
    }

    pub fn params(&self) -> &[Symbol] {
        &self.params
    }

    pub fn body(&self) -> &Expr {
        &self.body
    }
}
