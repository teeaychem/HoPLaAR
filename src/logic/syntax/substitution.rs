use crate::logic::{Atomic, Formula, OpBinary, OpUnary};

impl<A: Atomic> Formula<A> {
    // Applies a single instance of simplification to `self`
    pub fn simplify_once(mut self) -> Self {
        use {Formula::*, OpBinary::*, OpUnary::*, std::mem::take};
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
                        Not => take(expr),
                    },

                    _ => self,
                },
            },

            Binary { op, lhs, rhs } => match op {
                And => match (lhs.as_mut(), rhs.as_mut()) {
                    (False, _) | (_, False) => False,
                    (True, expr) | (expr, True) => take(expr),
                    _ => self,
                },

                Or => match (lhs.as_mut(), rhs.as_mut()) {
                    (True, _) | (_, True) => True,
                    (False, expr) | (expr, False) => take(expr),
                    _ => self,
                },

                Imp => match (lhs.as_mut(), rhs.as_mut()) {
                    (False, _) | (_, True) => True,
                    (True, expr) => take(expr),
                    (expr, False) => Formula::Not(expr.clone()),

                    _ => self,
                },

                Iff => match (lhs.as_mut(), rhs.as_mut()) {
                    (expr, True) | (True, expr) => take(expr),
                    (expr, False) | (False, expr) => Formula::Not(take(expr)),
                    _ => self,
                },
            },

            Quantified { var, fm, .. } => {
                if fm.free_variables().iter().any(|v| v == var) {
                    return self;
                }

                take(fm)
            }

            _ => self,
        }
    }

    pub fn simplify(self) -> Self {
        match self {
            Formula::Unary { op, expr } => Formula::Unary(op, expr.simplify()).simplify_once(),
            Formula::Binary { op, lhs, rhs } => {
                Formula::Binary(op, lhs.simplify(), rhs.simplify()).simplify_once()
            }

            Formula::Quantified { q, var, fm: expr } => {
                Formula::Quantified(q, var, expr.simplify()).simplify_once()
            }

            _ => self,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, parse_first_order, parse_propositional};

    #[test]
    fn simplification() {
        let expr = parse_propositional("(true ==> (x <=> false)) ==> ~(y | false & z)");
        let expected = parse_propositional("~x ==> ~y");

        assert_eq!(expr.simplify(), expected);

        let expr = parse_propositional("((x ==> y) ==> true) | ~false");
        let expected = parse_propositional("true");

        assert_eq!(expr.simplify(), expected);
    }

    #[test]
    fn simplification_quantifier() {
        let expr = parse_first_order("forall x. eq(P(a), P(b))");
        let expected = parse_first_order("eq(P(a), P(b))");

        assert_eq!(expr.simplify(), expected);

        let expr = parse_first_order("forall x. (P(a) | ~P(x))");
        let expected = expr.clone();

        assert_eq!(expr.simplify(), expected);

        let expr = parse_first_order("forall x. (P(a) | true)");
        let expected = Formula::True;

        assert_eq!(expr.simplify(), expected);
    }
}
