use crate::logic::{Atomic, Formula, OpUnary};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Literal<A> {
    atom: A,
    value: bool,
}

impl<A: Atomic> Literal<A> {
    pub fn from(atom: A, value: bool) -> Self {
        Literal { atom, value }
    }

    pub fn as_formula(&self) -> Formula<A> {
        match self.value {
            true => Formula::Atom(self.atom.to_owned()),
            false => Formula::Not(Formula::Atom(self.atom.to_owned())),
        }
    }

    pub fn atom(&self) -> &A {
        &self.atom
    }

    pub fn id(&self) -> &str {
        self.atom.id()
    }

    pub fn value(&self) -> bool {
        self.value
    }
}

impl<A: Atomic> Literal<A> {
    pub fn set_value(&mut self, value: bool) {
        self.value = value
    }

    pub fn invert_value(&mut self) {
        self.value = !self.value
    }
}

impl<A: Atomic> Literal<A> {
    pub fn negate(mut self) -> Self {
        self.value = !self.value;
        self
    }
}

impl<A: Atomic> std::cmp::PartialOrd for Literal<A> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<A: Atomic> std::cmp::Ord for Literal<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        match self.atom.cmp(&other.atom) {
            Less => Less,
            Greater => Greater,
            Equal => self.value.cmp(&other.value),
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
