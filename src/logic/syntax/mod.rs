pub mod normal_form;
pub mod substitution;

use crate::logic::{Atomic, Formula, OpBinary, OpUnary, Quantifier};

impl<A: Atomic> Formula<A> {
    pub fn dual(self) -> Formula<A> {
        match self {
            Formula::True => Formula::False,
            Formula::False => Formula::True,

            Formula::Atom { .. } => self,

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => Formula::Not(expr.dual()),
            },

            Formula::Binary { op, lhs, rhs } => match op {
                OpBinary::And => Formula::Or(lhs.dual(), rhs.dual()),
                OpBinary::Or => Formula::And(lhs.dual(), rhs.dual()),
                OpBinary::Imp => panic!("Dual of Imp"),
                OpBinary::Iff => panic!("Dual of Iff"),
            },

            Formula::Quantified { q, var, expr } => match q {
                Quantifier::ForAll => Formula::Exists(var, expr.dual()),
                Quantifier::Exists => Formula::ForAll(var, expr.dual()),
            },
        }
    }
}

impl<A: Atomic> Formula<A> {
    pub fn negate(self) -> Self {
        !self
    }
}

impl<A: Atomic> std::ops::Not for Formula<A> {
    type Output = Formula<A>;

    fn not(self) -> Self::Output {
        match self {
            Formula::True => Formula::False,
            Formula::False => Formula::True,

            Formula::Atom { .. } => Formula::Not(self),

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => *expr,
            },

            Formula::Binary { .. } => Formula::Not(self),

            Formula::Quantified { .. } => Formula::Not(self),
        }
    }
}

impl<A: Atomic> Formula<A> {
    pub fn distribute(mut self) -> Self {
        use {Formula::*, OpBinary::*, std::mem::take};

        match &mut self {
            Binary {
                op: And,
                lhs: a,
                rhs: b,
            } => match (a.as_mut(), b.as_mut()) {
                (_, Binary { op: Or, lhs, rhs }) => {
                    let a = *take(a);

                    let lhs = Formula::And(a.clone(), *take(lhs));
                    let rhs = Formula::And(a, *take(rhs));

                    Formula::Or(lhs.distribute(), rhs.distribute())
                }

                (Binary { op: Or, lhs, rhs }, _) => {
                    let b = *take(b);

                    let lhs = Formula::And(*take(lhs), b.clone());
                    let rhs = Formula::And(*take(rhs), b);

                    Formula::Or(lhs.distribute(), rhs.distribute())
                }

                _ => self,
            },

            _ => self,
        }
    }

    pub fn raw_dnf(mut self) -> Self {
        use {Formula::*, OpBinary::*, std::mem::take};

        match &mut self {
            Binary { op: And, lhs, rhs } => {
                Formula::And(take(lhs).raw_dnf(), take(rhs).raw_dnf()).distribute()
            }

            Binary { op: Or, lhs, rhs } => Formula::Or(take(lhs).raw_dnf(), take(rhs).raw_dnf()),

            _ => self,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, parse_propositional, propositional::Valuation};

    #[test]
    fn duals() {
        let expr = parse_propositional("p | ~ p");
        let expected = parse_propositional("p & ~ p");
        assert_eq!(expr.dual(), expected);
    }

    #[test]
    fn thm_2_7() {
        let expr = parse_propositional("p & q");
        let v_a = Valuation::from_prop_set(expr.atoms());
        assert_eq!(expr.eval(&v_a), !expr.dual().eval(&v_a.inverted()))
    }

    #[test]
    fn literals() {
        let p = parse_propositional("p");

        assert!(p.is_literal());
        assert!(p.is_positive_literal());
        assert!(!p.is_negative_literal());

        let pq = parse_propositional("p & q");
        assert!(!pq.is_literal());
        assert!(!pq.is_positive_literal());
        assert!(!pq.is_negative_literal());
    }

    #[test]
    fn negate() {
        let p = parse_propositional("p");
        let n = parse_propositional("~p");

        assert_eq!(n.negate(), p);

        let p = parse_propositional("p & q");
        let n = parse_propositional("~(p & q)");

        assert_eq!(n.negate(), p);
    }

    #[test]
    fn distribution() {
        let pqr = parse_propositional("p & (q | r)");
        let expected = parse_propositional("(p & q) | (p & r)");
        assert_eq!(pqr.distribute(), expected);

        let pqr = parse_propositional("(p | q) & r");
        let expected = parse_propositional("(p & r) | (q & r)");
        assert_eq!(pqr.distribute(), expected);
    }

    #[test]
    fn raw_dnf() {
        let expr = parse_propositional("(p | q & r) & (~p | ~r)");
        let expected = parse_propositional("(p & ~p | (q & r) & ~p) | p & ~r | (q & r) & ~r");
        assert!(Formula::Iff(expr.raw_dnf(), expected).tautology());
    }
}
