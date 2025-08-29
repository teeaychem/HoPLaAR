mod atoms;

pub use atoms::on_atoms;

mod display;

pub mod formula_set;
pub mod iterators;

mod literal;
pub use literal::Literal;

mod normal_form;

mod parsing;
pub use parsing::parse_propositional_formula;

pub mod propositional;

pub mod utils;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum OpUnary {
    Not,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum OpBinary {
    And,
    Or,
    Imp,
    Iff,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Quantifier {
    ForAll,
    Exists,
}

pub trait Atomic:
    std::fmt::Debug + std::fmt::Display + Clone + std::hash::Hash + Eq + std::cmp::Ord
{
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
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

#[allow(clippy::derivable_impls)]
impl<A: Atomic> Default for Formula<A> {
    fn default() -> Self {
        Formula::False
    }
}

#[allow(non_snake_case)]
impl<A: Atomic> Formula<A> {
    pub fn Unary(op: OpUnary, expr: Formula<A>) -> Self {
        Self::Unary {
            op,
            expr: Box::new(expr),
        }
    }

    pub fn Binary(op: OpBinary, lhs: Formula<A>, rhs: Formula<A>) -> Self {
        Self::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn Quantifier(q: Quantifier, var: A, expr: Formula<A>) -> Self {
        Self::Quantifier {
            q,
            var,
            expr: Box::new(expr),
        }
    }
}

#[allow(non_snake_case)]
impl<A: Atomic> Formula<A> {
    pub fn Not(expr: Formula<A>) -> Self {
        Self::Unary(OpUnary::Not, expr)
    }

    pub fn And(lhs: Formula<A>, rhs: Formula<A>) -> Self {
        Self::Binary(OpBinary::And, lhs, rhs)
    }

    pub fn Or(lhs: Formula<A>, rhs: Formula<A>) -> Self {
        Self::Binary(OpBinary::Or, lhs, rhs)
    }

    pub fn Imp(lhs: Formula<A>, rhs: Formula<A>) -> Self {
        Self::Binary(OpBinary::Imp, lhs, rhs)
    }

    pub fn Iff(lhs: Formula<A>, rhs: Formula<A>) -> Self {
        Self::Binary(OpBinary::Iff, lhs, rhs)
    }

    pub fn Atom(var: A) -> Self {
        Self::Atom { var }
    }

    pub fn Exists(var: A, expr: Formula<A>) -> Self {
        Self::Quantifier(Quantifier::Exists, var, expr)
    }

    pub fn ForAll(var: A, expr: Formula<A>) -> Self {
        Self::Quantifier(Quantifier::ForAll, var, expr)
    }
}

impl<A: Atomic> Formula<A> {
    pub fn conjoin<I: Iterator<Item = Formula<A>>>(mut conjuncts: I) -> Formula<A> {
        let mut formula = match conjuncts.next() {
            Some(conjunct) => conjunct,
            None => Formula::True,
        };

        for conjunct in conjuncts {
            formula = Formula::And(formula, conjunct)
        }

        formula
    }

    pub fn disjoin<I: Iterator<Item = Formula<A>>>(mut conjuncts: I) -> Formula<A> {
        let mut formula = match conjuncts.next() {
            Some(conjunct) => conjunct,
            None => Formula::True,
        };

        for conjunct in conjuncts {
            formula = Formula::Or(formula, conjunct)
        }

        formula
    }
}
