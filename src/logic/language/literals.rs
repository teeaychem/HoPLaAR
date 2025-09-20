use crate::logic::{Atomic, Formula, OpUnary};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Literal<A> {
    /// An atom, which typically implemenets the Atomic trait.
    atom: A,

    /// The value (or polarity) of the literal.
    value: bool,
}

impl<A: Atomic> Literal<A> {
    pub fn from(atom: A, value: bool) -> Self {
        Literal { atom, value }
    }

    /// The atom of `self`.
    pub fn atom(&self) -> &A {
        &self.atom
    }

    /// The (atom) id of `self`.
    pub fn id(&self) -> &str {
        self.atom.id()
    }

    /// The value of `self`.
    pub fn value(&self) -> bool {
        self.value
    }
}

impl<A: Atomic> Literal<A> {
    /// A mutable borrow of the literal's atom.
    pub fn atom_mut(&mut self) -> &mut A {
        &mut self.atom
    }

    /// Set the value of the literal to `value`.
    pub fn set_value(&mut self, value: bool) {
        self.value = value
    }

    /// Invert the literal's value.
    /// That is, from true to false, or from false to true.
    pub fn invert_value(&mut self) {
        self.value = !self.value
    }
}

impl<A: Atomic> Literal<A> {
    /// Returns a negated instance of the literal.
    pub fn negate(mut self) -> Self {
        self.invert_value();
        self
    }
}

// std::etc...

impl<A: Atomic> std::cmp::PartialOrd for Literal<A> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<A: Atomic> std::cmp::Ord for Literal<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        match self.value.cmp(&other.value) {
            Less => Less,
            Greater => Greater,
            Equal => self.atom.cmp(&other.atom),
        }
    }
}

impl<A: Atomic> std::fmt::Display for Literal<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            true => write!(f, "{}", self.atom),
            false => write!(f, "¬{}", self.atom),
        }
    }
}

impl<A: Atomic> From<Literal<A>> for Formula<A> {
    fn from(value: Literal<A>) -> Self {
        match value.value {
            true => Formula::Atom(value.atom.to_owned()),
            false => Formula::Not(Formula::Atom(value.atom.to_owned())),
        }
    }
}

impl<A: Atomic> Formula<A> {
    pub fn is_literal(&self) -> bool {
        match self {
            Formula::Atom { .. } => true,

            Formula::Unary { op, expr } => {
                op == &OpUnary::Not && matches!(expr.as_ref(), Formula::Atom { .. })
            }

            _ => false,
        }
    }

    pub fn is_positive_literal(&self) -> bool {
        matches!(self, Formula::Atom { .. })
    }

    pub fn is_negative_literal(&self) -> bool {
        match &self {
            Formula::Unary { op, expr } => op == &OpUnary::Not && expr.is_positive_literal(),

            _ => false,
        }
    }
}
