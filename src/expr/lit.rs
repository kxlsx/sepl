use std::hash::Hash;
use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum Lit {
    Float(f64),
    Bool(bool),
    Nil,
}

impl PartialEq for Lit {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lit::Bool(a), Lit::Bool(b)) => a == b,
            (Lit::Float(a), Lit::Float(b)) => a.to_bits() == b.to_bits(),
            _ => false
        }
    }
}

impl Eq for Lit { }

impl Hash for Lit {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Lit::Bool(bool) => bool.hash(state),
            Lit::Float(float) => float.to_bits().hash(state),
            Lit::Nil => Lit::Nil.hash(state),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Float(float) => float.fmt(f),
            Lit::Bool(bool) => bool.fmt(f),
            Lit::Nil => "nil".fmt(f),
        }
    }
}
