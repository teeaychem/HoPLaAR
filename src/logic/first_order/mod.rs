mod terms;
use std::collections::HashMap;

pub use terms::{Term, TermId};

mod relations;
pub use relations::Relation;

use crate::logic::{
    Formula,
    first_order::terms::{Fun, Var},
};

pub type FirstOrderFormula = Formula<Relation>;

#[derive(Clone)]
pub struct Interpretation<Domain> {
    functions: fn(&Fun, &Valuation<Domain>) -> Domain,
    relations: HashMap<Relation, Domain>,
}

pub type Valuation<Domain> = HashMap<Var, Domain>;
pub type InterpretationF<Domain> = fn(&Fun, &Valuation<Domain>) -> Domain;

pub type InterpretationBool = Interpretation<bool>;

pub trait Domain: std::fmt::Debug + std::fmt::Display + Clone {}

impl Domain for bool {}

impl<D: Domain> Interpretation<D> {
    pub fn term_value(&self, term: &Term, valuation: &Valuation<D>) -> D {
        match term {
            Term::F(fun) => (self.functions)(fun, valuation),
            Term::V(var) => valuation[var].clone(),
        }
    }
}

impl Term {
    pub fn eval<D: Domain>(
        &self,
        interpretation: InterpretationF<D>,
        valuation: &Valuation<D>,
    ) -> D {
        match self {
            Term::F(fun) => interpretation(fun, valuation),
            Term::V(var) => valuation[var].clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::logic::{
        first_order::{
            Valuation,
            terms::{Fun, Var},
        },
        parse::try_parse_term,
    };

    fn interpretation_bool_functions(fun: &Fun, valuation: &Valuation<bool>) -> bool {
        match (fun.id(), fun.args()) {
            ("0", []) => false,
            ("1", []) => true,
            ("add", [x, y]) => {
                let x_val = x.eval(interpretation_bool_functions, valuation);
                let y_val = y.eval(interpretation_bool_functions, valuation);
                x_val && y_val
            }

            _ => todo!(),
        }
    }

    #[test]
    fn valuation_basic() {
        let valuation = HashMap::from([(Var::from("x"), true), (Var::from("y"), true)]);
        let expr = try_parse_term("add(x, y)").unwrap();
        let v = expr.eval(interpretation_bool_functions, &valuation);
        dbg!(&expr);

        println!("{expr} {v}");

        // let bool_interpretation = InterpretationBool::default();
    }
}
