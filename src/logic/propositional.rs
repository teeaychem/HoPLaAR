use std::collections::HashMap;

use crate::logic::{Atomic, Formula};

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq)]
pub struct Prop {
    name: String,
}

impl Atomic for Prop {}

impl Prop {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn name_set(&mut self, name: String) {
        self.name = name
    }
}

// Propositional formula
pub type PropFormula = Formula<Prop>;

pub type Valuation = HashMap<Prop, bool>;

impl PropFormula {
    pub fn eval(&self, valuation: &Valuation) -> bool {
        match self {
            Formula::True => true,

            Formula::False => false,

            Formula::Atom { var } => valuation[var],

            Formula::OpUnary { op, expr } => match op {
                crate::logic::OpUnary::Not => !expr.eval(valuation),
            },

            Formula::OpBinary { op, lhs, rhs } => match op {
                crate::logic::OpBinary::And => lhs.eval(valuation) && rhs.eval(valuation),
                crate::logic::OpBinary::Or => lhs.eval(valuation) || rhs.eval(valuation),
                crate::logic::OpBinary::Imp => !lhs.eval(valuation) || rhs.eval(valuation),
                crate::logic::OpBinary::Iff => lhs.eval(valuation) == rhs.eval(valuation),
            },

            Formula::Quantifier { .. } => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{
        parsing::parse_propositional_formula,
        propositional::{Prop, Valuation},
    };

    #[test]
    fn eval_empty() {
        let v = Valuation::default();

        let expr = parse_propositional_formula("true or false");
        assert!(expr.eval(&v));

        let expr = parse_propositional_formula("true and false");
        assert!(!expr.eval(&v));
    }

    #[test]
    fn eval_small() {
        let v = Valuation::from([(Prop::new("a"), true), (Prop::new("b"), false)]);

        let expr = parse_propositional_formula("a or b");
        assert!(expr.eval(&v));

        let expr = parse_propositional_formula("a and b");
        assert!(!expr.eval(&v));

        let expr = parse_propositional_formula("a ==> b");
        assert!(!expr.eval(&v));

        let expr = parse_propositional_formula("~a ==> b");
        assert!(expr.eval(&v));

        let expr = parse_propositional_formula("~(a <=> b)");
        assert!(expr.eval(&v));
    }
}
