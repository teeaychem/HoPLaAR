pub mod normal_form;
pub mod substitution;

use crate::logic::{Atomic, Formula, OpBinary, OpUnary, Quantifier};

impl<A: Atomic> Formula<A> {
    pub fn dual(self) -> Formula<A> {
        match self {
            Formula::True => Formula::False,
            Formula::False => Formula::True,

            Formula::Atom { .. } => self,

            Formula::Unary { op, fml: expr } => match op {
                OpUnary::Not => Formula::Not(expr.dual()),
            },

            Formula::Binary { op, lhs, rhs } => match op {
                OpBinary::And => Formula::Or(lhs.dual(), rhs.dual()),
                OpBinary::Or => Formula::And(lhs.dual(), rhs.dual()),
                OpBinary::Imp => panic!("Dual of Imp"),
                OpBinary::Iff => panic!("Dual of Iff"),
            },

            Formula::Quantified { q, var, fml: fm } => match q {
                Quantifier::ForAll => Formula::Exists(var, fm.dual()),
                Quantifier::Exists => Formula::ForAll(var, fm.dual()),
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
        use OpUnary::*;

        match self {
            Formula::True => Formula::False,
            Formula::False => Formula::True,

            Formula::Atom { .. } => Formula::Not(self),

            Formula::Unary { op: Not, fml: expr } => *expr,

            Formula::Binary { .. } => Formula::Not(self),

            Formula::Quantified { .. } => Formula::Not(self),
        }
    }
}

impl<A: Atomic> Formula<A> {
    pub fn distribute(mut self) -> Self {
        use {Formula::*, OpBinary::*};

        match &mut self {
            Binary { op: And, lhs, rhs } => {
                // Take ownership of the side pointers.
                // If no distibution happens, these are to be replaced.
                let mut outer_lhs = std::mem::take(lhs);
                let mut outer_rhs = std::mem::take(rhs);

                match (&mut *outer_lhs, &mut *outer_rhs) {
                    (_, Binary { op: Or, lhs, rhs }) => {
                        let fresh_lhs = Formula::And(*outer_lhs.clone(), std::mem::take(lhs));
                        let fresh_rhs = Formula::And(*outer_lhs, std::mem::take(rhs));

                        Formula::Or(fresh_lhs.distribute(), fresh_rhs.distribute())
                    }

                    (Binary { op: Or, lhs, rhs }, _) => {
                        let fresh_lhs = Formula::And(std::mem::take(lhs), *outer_rhs.clone());
                        let fresh_rhs = Formula::And(std::mem::take(rhs), *outer_rhs);

                        Formula::Or(fresh_lhs.distribute(), fresh_rhs.distribute())
                    }

                    _ => {
                        *lhs = outer_lhs;
                        *rhs = outer_rhs;
                        self
                    }
                }
            }

            _ => self,
        }
    }

    pub fn simple_dnf(mut self) -> Self {
        use {Formula::*, OpBinary::*};

        match &mut self {
            Binary { op: And, lhs, rhs } => {
                let lhs = std::mem::take(lhs).simple_dnf();
                let rhs = std::mem::take(rhs).simple_dnf();
                Formula::And(lhs, rhs).distribute()
            }

            Binary { op: Or, lhs, rhs } => {
                let lhs = std::mem::take(lhs).simple_dnf();
                let rhs = std::mem::take(rhs).simple_dnf();
                Formula::Or(lhs, rhs)
            }

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
        assert!(p.negate().is_negative_literal());

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
        let pqr = parse_propositional("p & (q | r)").distribute();
        let expected = parse_propositional("(p & q) | (p & r)");
        assert_eq!(pqr, expected);

        let pqr = parse_propositional("(p | q) & r").distribute();
        let expected = parse_propositional("(p & r) | (q & r)");
        assert_eq!(pqr, expected);
    }

    #[test]
    fn raw_dnf() {
        let expr = parse_propositional("(p | q & r) & (~p | ~r)");
        let expected = parse_propositional("(p & ~p | (q & r) & ~p) | p & ~r | (q & r) & ~r");
        assert!(Formula::Iff(expr.simple_dnf(), expected).is_tautology());
    }
}
