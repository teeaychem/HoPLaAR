mod atoms;
pub use atoms::on_atoms;

mod display;
pub mod iterators;

mod parsing;
pub use parsing::parse_propositional_formula;

pub mod propositional;

pub mod utils;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum OpUnary {
    Not,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum OpBinary {
    And,
    Or,
    Imp,
    Iff,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Quantifier {
    ForAll,
    Exists,
}

pub trait Atomic: std::fmt::Debug + std::fmt::Display + Clone + std::hash::Hash + Eq {}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Formula<T: Atomic> {
    True,
    False,

    Atom {
        var: T,
    },

    Unary {
        op: OpUnary,
        expr: Box<Formula<T>>,
    },

    Binary {
        op: OpBinary,
        lhs: Box<Formula<T>>,
        rhs: Box<Formula<T>>,
    },

    Quantifier {
        q: Quantifier,
        var: T,
        expr: Box<Formula<T>>,
    },
}

#[allow(non_snake_case)]
impl<T: Atomic> Formula<T> {
    pub fn Unary(op: OpUnary, expr: Formula<T>) -> Self {
        Self::Unary {
            op,
            expr: Box::new(expr),
        }
    }

    pub fn Binary(op: OpBinary, lhs: Formula<T>, rhs: Formula<T>) -> Self {
        Self::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn Quantifier(q: Quantifier, var: T, expr: Formula<T>) -> Self {
        Self::Quantifier {
            q,
            var,
            expr: Box::new(expr),
        }
    }
}

#[allow(non_snake_case)]
impl<T: Atomic> Formula<T> {
    pub fn Not(expr: Formula<T>) -> Self {
        Self::Unary(OpUnary::Not, expr)
    }

    pub fn And(lhs: Formula<T>, rhs: Formula<T>) -> Self {
        Self::Binary(OpBinary::And, lhs, rhs)
    }

    pub fn Or(lhs: Formula<T>, rhs: Formula<T>) -> Self {
        Self::Binary(OpBinary::Or, lhs, rhs)
    }

    pub fn Imp(lhs: Formula<T>, rhs: Formula<T>) -> Self {
        Self::Binary(OpBinary::Imp, lhs, rhs)
    }

    pub fn Iff(lhs: Formula<T>, rhs: Formula<T>) -> Self {
        Self::Binary(OpBinary::Iff, lhs, rhs)
    }

    pub fn Atom(var: T) -> Self {
        Self::Atom { var }
    }

    pub fn Exists(var: T, expr: Formula<T>) -> Self {
        Self::Quantifier(Quantifier::Exists, var, expr)
    }

    pub fn ForAll(var: T, expr: Formula<T>) -> Self {
        Self::Quantifier(Quantifier::ForAll, var, expr)
    }
}
