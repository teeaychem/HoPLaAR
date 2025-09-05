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

    #[rustfmt::skip]
    pub fn pull_quantifiers(mut self) -> FirstOrderFormula {
        use {Formula::*, OpBinary::*, Quantifier::*};

        // Any required variants are generated with respect to the free variables of the formula.
        // As the formula is to be deconstructed, the variables are cached here.
        let fv = self.free_variables();
        let mut substitution = Substitution::default();

        match self {
            Binary { op, ref mut lhs, ref mut rhs } => {
                let lhs = *std::mem::take(&mut *lhs);
                let rhs = *std::mem::take(&mut *rhs);

                match (op, lhs.clone(), rhs.clone()) {
                    (
                        And,
                        Quantified { q: ForAll, var: x, fm: p },
                        Quantified { q: ForAll, var: y, fm: q },
                    ) => {
                        let z = x.fresh_variant(fv.iter());

                        substitution.add_interrupt(&x, Some(Term::V(z.clone())));
                        let fresh_p = p.term_substitution(&mut substitution);
                        substitution.remove_interrupt(&x);

                        substitution.add_interrupt(&y, Some(Term::V(z.clone())));
                        let fresh_q = q.term_substitution(&mut substitution);

                        Formula::ForAll(z, Formula::And(fresh_p, fresh_q).pull_quantifiers())
                    }

                    (
                        Or,
                        Quantified { q: Exists, var: x, fm: p },
                        Quantified { q: Exists, var: y, fm: q },
                    ) => {
                        let z = x.fresh_variant(fv.iter());
                        substitution.add_interrupt(&x, Some(Term::V(z.clone())));
                        let fresh_p = p.term_substitution(&mut substitution);
                        substitution.remove_interrupt(&x);

                        substitution.add_interrupt(&y, Some(Term::V(z.clone())));
                        let fresh_q = q.term_substitution(&mut substitution);

                        Formula::Exists(z, Formula::Or(fresh_p, fresh_q).pull_quantifiers())
                    }

                    // Any binary with only a single quantified side allows pulling the quantifier to the front.
                    // Still, these are split into cases to preserve the order of sides.
                    (_, Quantified { q, var, fm }, unquantified) => {
                        let z = var.fresh_variant(fv.iter());
                        substitution.add_interrupt(&var, Some(Term::V(z.clone())));
                        let fresh_fm = fm.term_substitution(&mut substitution);

                        Formula::Quantified(q, z, Formula::Binary(op, fresh_fm, unquantified).pull_quantifiers())
                    }

                    (_, unquantified, Quantified { q, var, fm }) => {
                        let z = var.fresh_variant(fv.iter());
                        substitution.add_interrupt(&var, Some(Term::V(z.clone())));
                        let fresh_fm = fm.term_substitution(&mut substitution);

                        Formula::Quantified(q, z, Formula::Binary(op, unquantified, fresh_fm).pull_quantifiers())
                    }

                    _ => Formula::Binary(op, lhs, rhs),
                }
            }

            _ => self,
        }
    }

    pub fn prenex(mut self) -> FirstOrderFormula {
        match self {
            Formula::True | Formula::False | Formula::Atom(_) | Formula::Unary { .. } => self,
            Formula::Binary {
                op,
                ref mut lhs,
                ref mut rhs,
            } => match op {
                OpBinary::And | OpBinary::Or => {
                    let lhs = std::mem::take(lhs);
                    let rhs = std::mem::take(rhs);
                    Formula::Binary(op, lhs.prenex(), rhs.prenex()).pull_quantifiers()
                }
                OpBinary::Imp | OpBinary::Iff => self,
            },
            Formula::Quantified { q, var, fm } => Formula::Quantified(q, var, fm.prenex()),
        }
    }

    pub fn prenex_normal_form(self) -> FirstOrderFormula {
        self.simplify().nnf().prenex()
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

        // Ensuring free variables are sorted seems unreasonable.
        assert!(generalisation == q_expr_xy || generalisation == q_expr_yx)
    }

    #[test]
    fn pulls() {
        let fm = parse("P(x) | forall x. ~P(x)");
        let pulled = fm.pull_quantifiers();

        let expected = parse("forall x_1. (P(x) |  ~P(x_1))");

        assert_eq!(pulled, expected);
    }

    #[test]
    fn prenex_normal_form() {
        let fm = parse(
            "(forall x. (P(x) | R(y))) => exists y. (exists z. (Q(y) | ~(exists z. (P(z) & Q(z)))))",
        );

        let pnf = fm.prenex_normal_form();

        let expected = parse("exists x. forall z. (~P(x) & ~R(y) | Q(x) | ~P(z) | ~Q(z))");

        assert_eq!(pnf, expected);
    }
}
