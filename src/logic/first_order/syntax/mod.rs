mod substitution;
pub use substitution::Substitution;

use crate::logic::{
    Formula, OpBinary, Quantifier,
    first_order::{FirstOrderFormula, Term},
};

impl FirstOrderFormula {
    pub fn generalise(self) -> FirstOrderFormula {
        let fv = self.free_variables();
        let mut formula = self;
        for var in fv {
            formula = Formula::Quantified(Quantifier::ForAll, var, formula);
        }
        formula
    }

    pub fn pull_quantifiers(mut self) -> FirstOrderFormula {
        use {Formula::*, OpBinary::*, Quantifier::*};

        // Any required variants are generated with respect to the free variables of the formula.
        // As the formula is to be deconstructed, the variables are cached here.
        let fv = self.free_variables();
        let mut substitution = Substitution::default();

        match self {
            Binary {
                op,
                ref mut lhs,
                ref mut rhs,
            } => {
                let lhs = *std::mem::take(&mut *lhs);
                let rhs = *std::mem::take(&mut *rhs);

                match (op, lhs, rhs) {
                    (
                        And,
                        Quantified {
                            q: ForAll,
                            var: x,
                            fm: p,
                        },
                        Quantified {
                            q: ForAll,
                            var: y,
                            fm: q,
                        },
                    ) => {
                        let z = x.variant(&fv);
                        substitution.add_interrupt(&x, Some(Term::V(z.clone())));
                        substitution.add_interrupt(&y, Some(Term::V(z.clone())));
                        let fresh_p = p.term_substitution(&mut substitution);
                        let fresh_q = q.term_substitution(&mut substitution);

                        Formula::ForAll(z, Formula::And(fresh_p, fresh_q))
                    }

                    (
                        Or,
                        Quantified {
                            q: Exists,
                            var: x,
                            fm: p,
                        },
                        Quantified {
                            q: Exists,
                            var: y,
                            fm: q,
                        },
                    ) => {
                        let z = x.variant(&fv);
                        substitution.add_interrupt(&x, Some(Term::V(z.clone())));
                        substitution.add_interrupt(&y, Some(Term::V(z.clone())));
                        let fresh_p = p.term_substitution(&mut substitution);
                        let fresh_q = q.term_substitution(&mut substitution);

                        Formula::Exists(z, Formula::Or(fresh_p, fresh_q))
                    }

                    (And, Quantified { q, var: x, fm: p }, unquantified) => {
                        let z = x.variant(&fv);
                        substitution.add_interrupt(&x, Some(Term::V(z.clone())));
                        let fresh_p = p.term_substitution(&mut substitution);

                        Formula::Quantified(q, z, Formula::And(unquantified, fresh_p))
                    }

                    (And, unquantified, Quantified { q, var: x, fm: p }) => {
                        let z = x.variant(&fv);
                        substitution.add_interrupt(&x, Some(Term::V(z.clone())));
                        let fresh_p = p.term_substitution(&mut substitution);

                        Formula::Quantified(q, z, Formula::And(fresh_p, unquantified))
                    }

                    (Or, Quantified { q, var: x, fm: p }, unquantified) => {
                        let z = x.variant(&fv);
                        substitution.add_interrupt(&x, Some(Term::V(z.clone())));
                        let fresh_p = p.term_substitution(&mut substitution);

                        Formula::Quantified(q, z, Formula::Or(fresh_p, unquantified))
                    }
                    (Or, unquantified, Quantified { q, var: x, fm: p }) => {
                        let z = x.variant(&fv);
                        substitution.add_interrupt(&x, Some(Term::V(z.clone())));
                        let fresh_p = p.term_substitution(&mut substitution);

                        Formula::Quantified(q, z, Formula::Or(unquantified, fresh_p))
                    }

                    _ => self,
                }
            }

            _ => self,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::first_order::parse;

    #[test]
    fn generalisation() {
        let expr = parse("R(x,y) => exists z. (R(x,z) & R(z,y))");
        let generalisation = expr.generalise();

        let q_expr_xy = parse("forall x. forall y. (R(x,y) => exists z. (R(x,z) & R(z,y)))");
        let q_expr_yx = parse("forall y. forall x. (R(x,y) => exists z. (R(x,z) & R(z,y)))");

        // Ensuring free variables are sorted seems an unreasonable cost.
        assert!(generalisation == q_expr_xy || generalisation == q_expr_yx)
    }

    #[test]
    fn pulls() {
        let fm = parse("P(x) | forall x. ~P(x)");
        let pulled = fm.pull_quantifiers();

        let expected = parse("forall x'. (P(x) |  ~P(x'))");
        assert_eq!(pulled, expected);
    }
}
