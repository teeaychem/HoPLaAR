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
            Formula::Quantifier { q, var, expr } => match q {
                Quantifier::ForAll => Formula::Exists(var, expr.dual()),
                Quantifier::Exists => Formula::ForAll(var, expr.dual()),
            },
        }
    }

    // Applies a single instance of simplification to `self`
    pub fn simplify_once(mut self) -> Self {
        use {Formula::*, OpBinary::*, OpUnary::*};
        // The internals are a little complex as the formula should only be changed when simplification occurs
        // So, the formula is borrowed for initial matches.
        // And, when some simplification occurs the enclosed expression is taken and the outer expression discarded.
        // To do this, self must be mutable, along with all borrows.

        match &mut self {
            Unary { op, expr } => match op {
                Not => match expr.as_mut() {
                    True => False,
                    False => True,

                    Unary { op, expr } => match op {
                        Not => std::mem::take(expr),
                    },

                    _ => self,
                },
            },

            Binary { op, lhs, rhs } => match op {
                And => match (lhs.as_mut(), rhs.as_mut()) {
                    (False, _) | (_, False) => False,
                    (True, expr) | (expr, True) => std::mem::take(expr),
                    _ => self,
                },

                Or => match (lhs.as_mut(), rhs.as_mut()) {
                    (True, _) | (_, True) => True,
                    (False, expr) | (expr, False) => std::mem::take(expr),
                    _ => self,
                },

                Imp => match (lhs.as_mut(), rhs.as_mut()) {
                    (False, _) | (_, True) => True,
                    (True, expr) => std::mem::take(expr),
                    (expr, False) => Formula::Not(expr.clone()),

                    _ => self,
                },

                Iff => match (lhs.as_mut(), rhs.as_mut()) {
                    (expr, True) | (True, expr) => std::mem::take(expr),
                    (expr, False) | (False, expr) => Formula::Not(std::mem::take(expr)),
                    _ => self,
                },
            },

            Quantifier { .. } => todo!(),

            _ => self,
        }
    }

    pub fn simplify(self) -> Self {
        match self {
            Formula::Unary { op, expr } => Formula::Unary(op, expr.simplify()).simplify_once(),
            Formula::Binary { op, lhs, rhs } => {
                Formula::Binary(op, lhs.simplify(), rhs.simplify()).simplify_once()
            }

            Formula::Quantifier { .. } => todo!(),

            _ => self,
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

            Formula::Quantifier { .. } => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{parsing::parse_propositional_formula, propositional::Valuation};

    #[test]
    fn duals() {
        let expr = parse_propositional_formula("p | ~ p");
        let expected = parse_propositional_formula("p & ~ p");
        assert_eq!(expr.dual(), expected);
    }

    #[test]
    fn thm_2_7() {
        let expr = parse_propositional_formula("p & q");
        let v_a = Valuation::from_prop_set(expr.atoms());
        assert_eq!(expr.eval(&v_a), !expr.dual().eval(&v_a.inverted()))
    }

    #[test]
    fn simplification() {
        let expr = parse_propositional_formula("(true ==> (x <=> false)) ==> ~(y | false & z)");
        let expected = parse_propositional_formula("~x ==> ~y");

        assert_eq!(expr.simplify(), expected);

        let expr = parse_propositional_formula("((x ==> y) ==> true) | ~false");
        let expected = parse_propositional_formula("true");

        assert_eq!(expr.simplify(), expected);
    }

    #[test]
    fn literals() {
        let p = parse_propositional_formula("p");

        assert!(p.is_literal());
        assert!(p.is_positive_literal());
        assert!(!p.is_negative_literal());

        let pq = parse_propositional_formula("p & q");
        assert!(!pq.is_literal());
        assert!(!pq.is_positive_literal());
        assert!(!pq.is_negative_literal());
    }

    #[test]
    fn negate() {
        let p = parse_propositional_formula("p");
        let n = parse_propositional_formula("~p");

        assert_eq!(n.negate(), p);

        let p = parse_propositional_formula("p & q");
        let n = parse_propositional_formula("~(p & q)");

        assert_eq!(n.negate(), p);
    }
}
