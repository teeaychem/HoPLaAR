mod substitution;
pub use substitution::Substitution;

use crate::logic::{Formula, Quantifier, first_order::FirstOrderFormula};

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
        use {Formula::*, Quantifier::*};

        // Any required variants are generated with respect to the free variables of the formula.
        // As the formula is to be deconstructed, the variables are cached here.
        let fv = self.free_variables();

        match self {
            Binary {
                op,
                ref mut lhs,
                ref mut rhs,
            } => {
                let lhs = *std::mem::take(&mut *lhs);
                let rhs = *std::mem::take(&mut *rhs);

                match (lhs, rhs) {
                    (
                        Quantified {
                            q: ForAll,
                            var: x,
                            expr: p,
                        },
                        Quantified {
                            q: ForAll,
                            var: y,
                            expr: q,
                        },
                    ) => {
                        todo!()
                    }

                    _ => self,
                }
            }

            _ => self,
        }
    }

    // fn pull_quantifier(
    //     fm: FirstOrderFormula,
    //     a: (bool, Var, FirstOrderFormula),
    //     b: (bool, Var, FirstOrderFormula),
    // ) -> (Var, FirstOrderFormula, FirstOrderFormula) {
    //     // if a.0 {
    //     //     let substitution = |t: Term| -> Term {
    //     //         match t {
    //     //             Term::V(a.1) =>
    //     //         }
    //     //     }
    //     // }

    //     todo!()
    // }
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
}
