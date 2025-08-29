use crate::logic::{
    Formula,
    propositional::{Prop, PropDict, PropFormula},
};

impl PropFormula {
    pub fn cnf(self) -> (Option<Prop>, PropFormula) {
        let mut dict = PropDict::default();
        let main_def = self.cnf_build_dict(&mut dict);

        if main_def == self {
            return (None, self);
        }

        match &main_def {
            PropFormula::Atom { var } => (Some(var.clone()), Formula::And(main_def, dict.to_cnf())),
            _ => panic!(),
        }
    }

    // Recursively replaces complex formulas with defining literals, and returns the defining literal.
    pub fn cnf_build_dict(&self, dict: &mut PropDict) -> PropFormula {
        use Formula::*;

        match self {
            True | False | Atom { .. } => self.clone(),

            Unary { op, expr } => {
                let expr = expr.cnf_build_dict(dict);
                PropFormula::Unary(*op, expr)
            }

            Binary { op, lhs, rhs } => {
                let lhs = lhs.cnf_build_dict(dict);
                let rhs = rhs.cnf_build_dict(dict);

                let defined = PropFormula::Binary(*op, lhs, rhs);

                let def = dict.get_or_insert(&defined);

                PropFormula::Atom(def)
            }

            Quantifier { .. } => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::parse_propositional_formula;

    #[test]
    fn cnf() {
        let expr = parse_propositional_formula("(p | (q & ~r))");
        let (_, cnf) = expr.clone().cnf();

        assert!(!expr.satisfiable() || cnf.satisfiable());
        assert!(!cnf.satisfiable() || expr.satisfiable());
    }
}
