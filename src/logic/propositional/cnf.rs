use crate::logic::{
    Formula,
    propositional::{Prop, PropDict, PropFormula},
};

impl PropFormula {
    pub fn cnf(self) -> (Option<Prop>, PropFormula) {
        let mut dict = PropDict::default();
        let main_def = self.cnf_build_binary_dict(&mut dict);

        if main_def == self {
            return (None, self);
        }

        match &main_def {
            PropFormula::Atom(atom) => (
                Some(atom.clone()),
                Formula::And(main_def, dict.to_three_cnf()),
            ),
            _ => panic!(),
        }
    }

    // Recursively replaces complex formulas with defining literals, and returns the defining literal.
    pub fn cnf_build_binary_dict(&self, dict: &mut PropDict) -> PropFormula {
        use Formula::*;

        match self {
            True | False | Atom { .. } => self.clone(),

            Unary { op, expr } => {
                let expr = expr.cnf_build_binary_dict(dict);
                PropFormula::Unary(*op, expr)
            }

            Binary { op, lhs, rhs } => {
                let lhs = lhs.cnf_build_binary_dict(dict);
                let rhs = rhs.cnf_build_binary_dict(dict);

                let defined = PropFormula::Binary(*op, lhs, rhs);

                let def = dict.get_or_insert(&defined);

                PropFormula::Atom(def)
            }

            Quantified { .. } => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::propositional::parse;

    #[test]
    fn cnf() {
        let expr = parse("(p | (q & ~r))");
        let (_, cnf) = expr.clone().cnf();

        assert!(!expr.is_satisfiable() || cnf.is_satisfiable());
        assert!(!cnf.is_satisfiable() || expr.is_satisfiable());
    }
}
