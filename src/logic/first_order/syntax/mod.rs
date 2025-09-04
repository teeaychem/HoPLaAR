mod substitution;

use crate::logic::{Formula, Quantifier, first_order::FirstOrderFormula};

impl FirstOrderFormula {
    pub fn generalise(self) -> FirstOrderFormula {
        let fv = self.free_variables();
        let mut formula = self;
        for var in fv {
            formula = Formula::Quantifier(Quantifier::ForAll, var, formula);
        }
        formula
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
