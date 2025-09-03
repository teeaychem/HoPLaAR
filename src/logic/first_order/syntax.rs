use crate::logic::{
    Formula, Quantifier,
    first_order::{FirstOrderFormula, Relation, Term},
};

impl FirstOrderFormula {
    pub fn generalise(self) -> FirstOrderFormula {
        let fv = self.free_variables();
        let mut formula = self;
        for var in fv {
            formula = Formula::Quantifier(Quantifier::ForAll, var, formula);
        }
        formula
    }

    pub fn term_substitution<S: Fn(Term) -> Term>(self, substitution: &S) -> FirstOrderFormula {
        match self {
            Formula::True | Formula::False => self,
            Formula::Atom(Relation { id, terms }) => {
                let fresh = Relation::from(id, terms.into_iter().map(substitution).collect());
                Formula::Atom(fresh)
            }
            Formula::Unary { op, expr } => Formula::Unary(op, expr.term_substitution(substitution)),
            Formula::Binary { op, lhs, rhs } => Formula::Binary(
                op,
                lhs.term_substitution(substitution),
                rhs.term_substitution(substitution),
            ),
            Formula::Quantifier { q, var, expr } => todo!(),
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
}
