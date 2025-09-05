mod substitution;
use std::collections::HashSet;

pub use substitution::Substitution;

use crate::logic::{
    Formula, OpBinary, Quantifier,
    first_order::{FirstOrderFormula, Term, terms::Fun},
};

/// Generalisation
impl FirstOrderFormula {
    pub fn generalize(self) -> FirstOrderFormula {
        let fv = self.free_variables();
        let mut formula = self;
        for var in fv {
            formula = Formula::Quantified(Quantifier::ForAll, var, formula);
        }
        formula
    }

    pub fn specialize(self) -> FirstOrderFormula {
        use Quantifier::*;
        match self {
            Formula::Quantified { q: ForAll, fm, .. } => fm.specialize(),
            _ => self,
        }
    }
}

/// Prenex normal form
impl FirstOrderFormula {
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
        self.simplify().negation_normal_form().prenex()
    }
}

/// Skolemization
impl FirstOrderFormula {
    fn skolem(self, taken: &mut HashSet<Fun>) -> FirstOrderFormula {
        match &self {
            Formula::True | Formula::False | Formula::Atom(_) | Formula::Unary { .. } => self,
            Formula::Binary { op, lhs, rhs } => {
                //
                match op {
                    OpBinary::And | OpBinary::Or => {
                        let lhs = lhs.clone().skolem(taken);
                        let rhs = rhs.clone().skolem(taken);
                        Formula::Binary(*op, lhs, rhs)
                    }

                    OpBinary::Imp | OpBinary::Iff => self,
                }
            }
            Formula::Quantified { q, var, fm } => {
                //
                match q {
                    Quantifier::ForAll => Formula::ForAll(var.clone(), fm.clone().skolem(taken)),
                    Quantifier::Exists => {
                        //
                        let xs = self.free_variables();

                        // var would include ANSI markup, so use var.id
                        let id = match xs.is_empty() {
                            true => format!("c_{}", var.id),
                            false => format!("f_{}", var.id),
                        };

                        let fresh_f = Fun {
                            id,
                            variant: 0,
                            args: xs.iter().map(|x| Term::V(x.clone())).collect(),
                        };

                        let fresh_f = fresh_f.fresh_variant(taken.iter());
                        taken.insert(fresh_f.clone());

                        let mut substitution = Substitution::default();
                        substitution.add_interrupt(var, Some(Term::F(fresh_f)));
                        let fm = fm.clone().term_substitution(&mut substitution);
                        fm.skolem(taken)
                    }
                }
            }
        }
    }

    pub fn skolemize_basic(self) -> FirstOrderFormula {
        let mut taken = self.functions();
        self.negation_normal_form().skolem(&mut taken)
    }

    pub fn skolemize(self) -> FirstOrderFormula {
        self.skolemize_basic().prenex_normal_form().specialize()
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::first_order::FirstOrderFormula;

    #[test]
    fn generalization() {
        let expr = FirstOrderFormula::from("R(x,y) => exists z. (R(x,z) & R(z,y))");
        let generalisation = expr.generalize();

        let q_expr_xy =
            FirstOrderFormula::from("forall x. forall y. (R(x,y) => exists z. (R(x,z) & R(z,y)))");
        let q_expr_yx =
            FirstOrderFormula::from("forall y. forall x. (R(x,y) => exists z. (R(x,z) & R(z,y)))");

        // Ensuring free variables are sorted seems unreasonable.
        assert!(generalisation == q_expr_xy || generalisation == q_expr_yx)
    }

    #[test]
    fn pulls() {
        let fm = FirstOrderFormula::from("P(x) | forall x. ~P(x)");
        let pulled = fm.pull_quantifiers();

        let expected = FirstOrderFormula::from("forall x_1. (P(x) |  ~P(x_1))");

        assert_eq!(pulled, expected);
    }

    #[test]
    fn prenex_normal_form() {
        let fm = FirstOrderFormula::from(
            "(forall x. (P(x) | R(y))) => exists y. (exists z. (Q(y) | ~(exists z. (P(z) & Q(z)))))",
        );

        let pnf = fm.prenex_normal_form();

        let expected =
            FirstOrderFormula::from("exists x. forall z. (~P(x) & ~R(y) | Q(x) | ~P(z) | ~Q(z))");

        assert_eq!(pnf, expected);
    }

    #[test]
    fn skolemization_a() {
        let fm = FirstOrderFormula::from(
            "exists y. (lt(x, y) => forall u. (exists v. lt(times(x, u), times(y, v))))",
        );

        let expected_xu =
            FirstOrderFormula::from("~lt(x, f_y(x)) | lt(times(x, u), times(f_y(x), f_v(x, u)))");
        let expected_ux =
            FirstOrderFormula::from("~lt(x, f_y(x)) | lt(times(x, u), times(f_y(x), f_v(u, x)))");

        let sk = fm.skolemize();

        if sk != expected_xu {
            assert_eq!(sk, expected_ux);
        } else {
            assert_eq!(sk, expected_xu);
        }
    }

    #[test]
    fn skolemization_b() {
        let fm = FirstOrderFormula::from(
            "forall x. (P(x) => exists y. exists z. (Q(y | ~(exists z. (P(z) & Q(z))))))",
        );
        // Note, c_y is a constant function
        let expected = FirstOrderFormula::from("~P(x) | Q(c_y()) | ~P(z) | ~Q(z)");

        let sk = fm.clone().skolemize();

        assert_eq!(sk, expected);
    }
}
