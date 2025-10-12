use std::{fmt::Display, hash::Hash};

/// Type representing any
/// value literal other than [`Symbol`](super::Symbol).
#[derive(Debug, Clone, Copy)]
pub enum Lit {
    Float(f64),
}

impl PartialEq for Lit {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lit::Float(a), Lit::Float(b)) => a.to_bits() == b.to_bits(),
        }
    }
}

impl Eq for Lit {}

impl Hash for Lit {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Lit::Float(float) => float.to_bits().hash(state),
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Float(float) => write!(f, "{}", float),
        }
    }
}
