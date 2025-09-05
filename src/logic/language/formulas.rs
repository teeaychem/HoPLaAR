use crate::logic::{Atomic, OpBinary, OpUnary, Quantifier};

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Formula<A: Atomic> {
    True,
    False,

    Atom(A),

    Unary {
        op: OpUnary,
        expr: Box<Formula<A>>,
    },

    Binary {
        op: OpBinary,
        lhs: Box<Formula<A>>,
        rhs: Box<Formula<A>>,
    },

    Quantified {
        q: Quantifier,
        var: A::Variable,
        fm: Box<Formula<A>>,
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

    pub fn Quantified(q: Quantifier, var: A::Variable, expr: Formula<A>) -> Self {
        Self::Quantified {
            q,
            var,
            fm: Box::new(expr),
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

    pub fn Atom(atomic: A) -> Self {
        Self::Atom(atomic)
    }

    pub fn Exists(var: A::Variable, expr: Formula<A>) -> Self {
        Self::Quantified(Quantifier::Exists, var, expr)
    }

    pub fn ForAll(var: A::Variable, expr: Formula<A>) -> Self {
        Self::Quantified(Quantifier::ForAll, var, expr)
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

impl<A: Atomic> std::fmt::Display for Formula<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Formula::True => write!(f, "⊤"),
            Formula::False => write!(f, "⊥"),

            Formula::Atom(atom) => write!(f, "{atom}"),

            Formula::Unary { op, expr } => match expr.as_ref() {
                Formula::True
                | Formula::False
                | Formula::Atom { .. }
                | Formula::Unary { .. }
                | Formula::Quantified { .. } => write!(f, "{op}{expr}"),

                Formula::Binary { .. } => write!(f, "{op}({expr})"),
            },

            Formula::Binary { op, lhs, rhs } => match (lhs.as_ref(), rhs.as_ref()) {
                (Formula::Binary { op: lop, .. }, Formula::Binary { op: rop, .. }) => {
                    if op == lop && op == rop {
                        write!(f, "{lhs} {op} {rhs}")
                    } else if op == lop {
                        write!(f, "{lhs} {op} ({rhs})")
                    } else if op == rop {
                        write!(f, "({lhs}) {op} {rhs}")
                    } else {
                        write!(f, "({lhs}) {op} ({rhs})")
                    }
                }

                (Formula::Binary { op: lop, .. }, _) => {
                    if op == lop {
                        write!(f, "{lhs} {op} {rhs}")
                    } else {
                        write!(f, "({lhs}) {op} {rhs}")
                    }
                }

                (_, Formula::Binary { op: rop, .. }) => {
                    if op == rop {
                        write!(f, "{lhs} {op} {rhs}")
                    } else {
                        write!(f, "{lhs} {op} ({rhs})")
                    }
                }

                _ => write!(f, "{lhs} {op} {rhs}"),
            },

            Formula::Quantified { q, var, fm: expr } => write!(f, "{q}{var}({expr})"),
        }
    }
}
