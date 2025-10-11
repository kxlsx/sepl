use std::collections::{hash_map::Iter as HashMapIter, HashMap};

use strum::IntoEnumIterator;

use crate::eval::{Builtin, Expr, Intern, Resolve, Symbol, SymbolTable};

/// Struct used to represent
/// a unique environment (or scope)
/// containing [`Symbol`] definitions.
///
/// [`Env`]s are returned by the [`EnvTable`]
/// using [`EnvTable::env_create`] and destroyed
/// using [`EnvTable::env_try_destroy`].
///
/// There is a special [`Env`] marked as `global`
/// that contains every other environment. It can
/// be obtained using [`Env::global`] or [`EnvTable::env_global`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Env(u64);

impl Env {
    pub const fn global() -> Self {
        EnvAllocator::env_global()
    }
}

/// Struct representing the current environment state,
/// containing all [`Symbol`]s and their definitions contained
/// in separate scopes.
///
/// Environments can be created using the [`env_create`](EnvTable::env_create)
/// method. An environment is represented as a unique [`Env`] struct,
/// [`Env::global`] being a special one, representing the root of every [`Env`].
/// Every [`Env`] must capture (i.e. inherit [`Symbol`] definitions from)
/// another [`Env`]. There is no automatic garbage collection, and every
/// environment must be deleted by the [`env_try_destroy`](EnvTable::env_try_destroy)
/// method (but not necessarily directly).
/// 
/// New [`Symbol`]s can be added using [`symbol_intern`](EnvTable::symbol_intern)
/// and they can be resolved to their names using [`symbol_resolve`](EnvTable::symbol_resolve).
///
/// [`Symbol`]s can be defined using the [`symbol_define`](EnvTable::symbol_define)
/// and [`symbol_define_global`](EnvTable::symbol_define_global) methods and are
/// automatically deleted when succesfully executing [`env_try_destroy`](EnvTable::env_try_destroy).
/// Definitions are extracted using the [`symbol_definition`](EnvTable::symbol_definition).
///
/// [`EnvTable`] can be initialized [`with_builtins`](EnvTable::with_builtins),
/// meaning every [`Builtin`] will have its [`Symbol`] representation
/// in the global [`Env`].
///
/// # Panics
/// It is considered an error to use an [`Env`] or a [`Symbol`] that's been either
/// destroyed or created using another [`EnvTable`]. Most methods
/// will [`panic`] in these situations.
///
/// # Example
/// ```
/// use sepl_lib::eval::{EnvTable, Expr, Lit};
///
/// let mut env_table = EnvTable::new();
/// let ten = env_table.symbol_intern("ten");
///
/// env_table.symbol_define_global(ten, Expr::Lit(Lit::Float(10.)));
///
/// assert_eq!(
///     env_table.symbol_definition(ten, env_table.env_global()),
///     Some(&Expr::Lit(Lit::Float(10.)))
/// );
/// ```
#[derive(Debug)]
pub struct EnvTable {
    env_allocator: EnvAllocator,
    env_tree: HashMap<Env, EnvTreeNode>,
    symbol_definitions: HashMap<Env, HashMap<Symbol, Expr>>,
    symbol_table: SymbolTable,
}

impl EnvTable {
    /// Create an [`EnvTable`] containing
    /// only the global environment with no definitions and [`Symbol`]s.
    pub fn new() -> Self {
        EnvTable {
            env_allocator: EnvAllocator::new(),
            env_tree: HashMap::from([(Env::global(), EnvTreeNode::global())]),
            symbol_definitions: HashMap::new(),
            symbol_table: SymbolTable::new(),
        }
    }

    /// Create an [`EnvTable`] containing every [`Builtin`]
    /// defined in the global environment. Every [`Builtin`]'s
    /// associated name is interned as a [`Symbol`].
    ///
    /// # Example
    /// ```
    ///
    /// use sepl_lib::eval::{EnvTable, Expr, Builtin, SymbolTable};
    ///
    /// let mut env_table = EnvTable::with_builtins();
    ///
    /// let lambda_symbol = env_table.symbol_intern("lambda");
    ///
    /// assert_eq!(
    ///     env_table.symbol_definition_global(lambda_symbol),
    ///     Some(&Expr::Builtin(Builtin::Lambda))
    /// )
    /// ```
    pub fn with_builtins() -> Self {
        let mut env_table = Self::new();

        for builtin in Builtin::iter() {
            let symbol = env_table.symbol_intern(builtin.as_ref());
            env_table.symbol_define_global(symbol, Expr::Builtin(builtin));
        }

        env_table
    }

    /// Intern a new [`Symbol`] in the internal [`SymbolTable`].
    /// See [`SymbolTable::intern`]. 
    pub fn symbol_intern(&mut self, name: &str) -> Symbol {
        self.symbol_table.intern(name)
    }

    /// Resolves the [`Symbol`]'s name in the internal [`SymbolTable`].
    /// See [`SymbolTable::resolve`]. 
    pub fn symbol_resolve(&self, symbol: Symbol) -> &str {
        self.symbol_table.resolve(symbol)
    }

    /// [`symbol_define`](EnvTable::symbol_define), but with `env` set as [`Env::global`].
    pub fn symbol_define_global(&mut self, symbol: Symbol, expr: Expr) -> Option<Expr> {
        self.symbol_define(symbol, self.env_global(), expr)
    }

    /// Define a [`Symbol`] as the passed [`Expr`] in the passed [`Env`].
    /// Returns the previous definition or [`None`].
    ///
    /// # Example
    /// ```
    /// use sepl_lib::eval::{EnvTable, Expr, Lit, SymbolTable};
    ///
    /// let mut env_table = EnvTable::new();
    /// let even = env_table.symbol_intern("this_is_even");
    ///
    /// env_table.symbol_define_global(even, Expr::Lit(Lit::Float(31.)));
    /// assert_eq!(
    ///     env_table.symbol_definition(even, env_table.env_global()),
    ///     Some(&Expr::Lit(Lit::Float(31.)))
    /// );
    ///
    /// let previous_def = env_table.symbol_define_global(even, Expr::Lit(Lit::Float(42.)));
    /// assert_eq!(previous_def, Some(Expr::Lit(Lit::Float(31.))));
    /// assert_eq!(
    ///     env_table.symbol_definition(even, env_table.env_global()),
    ///     Some(&Expr::Lit(Lit::Float(42.)))
    /// );
    /// ```
    pub fn symbol_define(&mut self, symbol: Symbol, env: Env, expr: Expr) -> Option<Expr> {
        if let Expr::Procedure(procedure) = &expr {
            self.increment_capturing_count(procedure.capture_env());
        }

        self.symbol_definitions
            .entry(env)
            .or_default()
            .insert(symbol, expr)
    }

    /// [`symbol_undefine`](EnvTable::symbol_undefine), but with `env` set as [`Env::global`].
    pub fn symbol_undefine_global(&mut self, symbol: Symbol) -> Option<Expr> {
        self.symbol_undefine(symbol, self.env_global())
    }

    /// Remove a symbol definition from a given [`Env`].
    /// Returns the removed definition if it existed.
    ///
    /// If the removed [`Expr`] is a [`Procedure`](super::Procedure),
    /// its capture_env may be removed from the [`EnvTable`]
    /// (if there are no more references to it).
    ///
    /// # Example
    /// ```
    /// use sepl_lib::eval::{EnvTable, Expr, Lit, SymbolTable};
    ///
    /// let mut env_table = EnvTable::new();
    /// let bomba = env_table.symbol_intern("bomba");
    ///
    /// env_table.symbol_define_global(bomba, Expr::Lit(Lit::Float(0.)));
    /// assert_eq!(
    ///     env_table.symbol_undefine_global(bomba),
    ///     Some(Expr::Lit(Lit::Float(0.)))
    /// );
    /// assert_eq!(
    ///     env_table.symbol_definition_global(bomba),
    ///     None
    /// );
    /// ```
    pub fn symbol_undefine(&mut self, symbol: Symbol, env: Env) -> Option<Expr> {
        let removed = self.symbol_definitions.get_mut(&env)?.remove(&symbol);

        if let Some(Expr::Procedure(procedure)) = &removed {
            self.decrement_capturing_count(procedure.capture_env());
        }

        removed
    }

    /// [`symbol_definition`](EnvTable::symbol_definition), but with `env` set as [`Env::global`].
    pub fn symbol_definition_global(&self, symbol: Symbol) -> Option<&Expr> {
        self.symbol_definition(symbol, self.env_global())
    }

    /// Returns the [`Symbol`]'s definition in the passed [`Env`].
    /// The definition is searched for recursively, checking
    /// the captured environment, the captured environment captured environment, etc.
    /// The search ends at [`Env::global`] and if the definition still cannot be found,
    /// [`None`] is returned.
    ///
    /// # Example
    /// ```
    /// use sepl_lib::eval::{EnvTable, Expr, Lit, SymbolTable, Env};
    ///
    /// let mut env_table = EnvTable::new();
    /// let env = env_table.env_create(Env::global());
    /// 
    /// let x = env_table.symbol_intern("x");
    /// let y = env_table.symbol_intern("y");
    ///
    /// env_table.symbol_define(x, Env::global(), Expr::Lit(Lit::Float(-128.)));
    /// env_table.symbol_define(x, env, Expr::Lit(Lit::Float(4096.)));
    ///
    /// assert_eq!(
    ///     env_table.symbol_definition(x, Env::global()),
    ///     Some(&Expr::Lit(Lit::Float(-128.)))
    /// );
    /// assert_eq!(
    ///     env_table.symbol_definition(x, env),
    ///     Some(&Expr::Lit(Lit::Float(4096.)))
    /// );
    /// assert_eq!(
    ///     env_table.symbol_definition(y, env),
    ///     None
    /// );
    /// ```
    pub fn symbol_definition(&self, symbol: Symbol, env: Env) -> Option<&Expr> {
        let definition_option = self
            .symbol_definitions
            .get(&env)
            .and_then(|symbol_defs| symbol_defs.get(&symbol));
        match (env, definition_option) {
            (env, result) if env == self.env_global() => result,
            (_, Some(expr)) => Some(expr),
            (env, None) => self.symbol_definition(symbol, self.get_captured_env(env)),
        }
    }

    /// Returns an iterator over every symbol and its
    /// definition in the global environment.
    pub fn symbols_global(&self) -> Option<HashMapIter<Symbol, Expr>> {
        self.symbol_definitions
            .get(&self.env_global())
            .map(|map| map.iter())
    }

    /// Returns `true` if the passed [`Env`]
    /// exists in the [`EnvTable`].
    pub fn env_exists(&self, env: Env) -> bool {
        self.env_tree.contains_key(&env)
    }

    /// Returns the global environment.
    pub const fn env_global(&self) -> Env {
        Env::global()
    }

    /// Returns the number of [`Env`]s
    /// in the [`EnvTable`].
    pub fn env_count(&self) -> usize {
        self.env_tree.len()
    }

    /// Create a new, unique [`Env`] capturing
    /// the passed environment.
    pub fn env_create(&mut self, captured_env: Env) -> Env {
        let env = self.env_allocator.env_alloc();
        self.insert_env(env, captured_env);
        env
    }

    /// Try to destroy the passed [`Env`] and
    /// the [`Env`]s it captures.
    ///
    /// Destroying fails if the environment
    /// is captured by any other [`Env`] or
    /// its bound to a [`Symbol`]
    /// (i.e. you cannot destroy a [`Procedure`](super::Procedure)'s
    /// environment if its bound to a [`Symbol`]
    /// or contains any other [`Procedure`](super::Procedure)).
    ///
    /// Returns true if destroying succeeded.
    pub fn env_try_destroy(&mut self, env: Env) -> bool {
        if env == Env::global() {
            return false;
        }
        if !self.env_tree.contains_key(&env) {
            return false;
        }
        if self.get_capturing_count(env) != 0 {
            return false;
        }

        let captured_env = self.get_captured_env(env);

        self.remove_node(env);
        self.decrement_capturing_count(captured_env);

        if let Some(symbol_defs) = self.symbol_definitions.remove(&env) {
            for (_, expr) in symbol_defs.iter() {
                if let Expr::Procedure(procedure) = expr {
                    self.decrement_capturing_count(procedure.capture_env())
                }
            }
        }

        self.env_allocator.env_free(env);

        true
    }

    /// Insert an [`EnvTreeNode`] corresponding to an
    /// [`Env`] into the tree.
    fn insert_env(&mut self, env: Env, captured_env: Env) {
        self.insert_node(
            env,
            EnvTreeNode {
                captured_env,
                capturing_count: 0,
            },
        );
        self.increment_capturing_count(captured_env);
    }

    /// Insert an [`EnvTreeNode`] into the tree.
    fn insert_node(&mut self, env: Env, node: EnvTreeNode) {
        match self.env_tree.insert(env, node) {
            None => (),
            Some(_) => panic!("Tried to insert existing Env."),
        }
    }

    /// Remove an [`Env`] from the tree.
    fn remove_node(&mut self, env: Env) {
        self.env_tree
            .remove(&env)
            .expect("Tried to remove non-existent Env");
    }

    /// Return the environment captured by the
    /// passed [`Env`].
    fn get_captured_env(&self, env: Env) -> Env {
        self.get_node(env).captured_env
    }

    /// Return the number of environment capturing the
    /// passed [`Env`].
    fn get_capturing_count(&self, env: Env) -> usize {
        self.get_node(env).capturing_count
    }

    /// Decrement the number of environments capturing
    /// the passed [`Env`]. If the `capturing_count`
    /// ends up as 0, [`env_try_destroy`](EnvTable::env_try_destroy)
    /// is called.
    fn decrement_capturing_count(&mut self, env: Env) {
        let node = self.get_node_mut(env);
        node.capturing_count -= 1;

        if node.capturing_count == 0 {
            self.env_try_destroy(env);
        }
    }

    /// Decrement the number of environments capturing
    /// the passed [`Env`].
    fn increment_capturing_count(&mut self, env: Env) {
        self.get_node_mut(env).capturing_count += 1
    }

    /// Get a reference to a node corresponding to the passed [`Env`].
    fn get_node(&self, env: Env) -> &EnvTreeNode {
        self.env_tree
            .get(&env)
            .expect("Tried to get non-existent Env.")
    }

    /// Get a mutable reference to a node corresponding to the passed [`Env`].
    fn get_node_mut(&mut self, env: Env) -> &mut EnvTreeNode {
        self.env_tree
            .get_mut(&env)
            .expect("Tried to get mutable non-existent Env.")
    }
}

impl Default for EnvTable {
    fn default() -> Self {
        Self::new()
    }
}

impl Resolve<Symbol> for EnvTable {
    fn resolve(&self, symbol: Symbol) -> &str {
        self.symbol_resolve(symbol)
    }
}

impl Intern<Symbol> for EnvTable {
    fn intern(&mut self, name: &str) -> Symbol {
        self.symbol_intern(name)
    }
}

/// Struct used to allocate
/// unique ID's to [`Env`]s
#[derive(Debug)]
struct EnvAllocator {
    last_id: u64,
    unused_ids: Vec<u64>,
}

impl EnvAllocator {
    /// Create a new [`EnvAllocator`].
    fn new() -> Self {
        Self {
            last_id: 1,
            unused_ids: Vec::new(),
        }
    }

    /// Return the global [`Env`].
    const fn env_global() -> Env {
        Env(0)
    }

    /// Allocate a new, unique [`Env`].
    fn env_alloc(&mut self) -> Env {
        if let Some(id) = self.unused_ids.pop() {
            Env(id)
        } else {
            let id = self.last_id;
            self.last_id += 1;
            Env(id)
        }
    }

    /// Return an [`Env`]'s ID to
    /// the allocator.
    fn env_free(&mut self, env: Env) {
        if env.0 == self.last_id - 1 {
            self.last_id -= 1;
        } else {
            self.unused_ids.push(env.0);
        }
    }
}

/// Struct representing capture
/// data corresponding to an [`Env`].
#[derive(Debug)]
struct EnvTreeNode {
    captured_env: Env,
    capturing_count: usize,
}

impl EnvTreeNode {
    /// Return an [`EnvTreeNode`] corresponding
    /// to [`Env::global`].
    fn global() -> Self {
        Self {
            captured_env: Env::global(),
            capturing_count: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    //TODO: EnvTable tests
}
