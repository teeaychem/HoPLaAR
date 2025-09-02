mod domains;
pub use domains::Domain;

mod terms;
use std::collections::HashMap;

pub use terms::{Term, TermId};

mod relations;
pub use relations::Relation;

use crate::logic::{
    Formula, OpBinary, OpUnary,
    first_order::terms::{Fun, Var},
};

pub use crate::logic::parse::parse_first_order as parse;

pub type FirstOrderFormula = Formula<Relation>;

pub type InterpretationF<D> = fn(&Fun, &Valuation<D>) -> D;
pub type InterpretationR<D> = fn(&Relation, InterpretationF<D>, &Valuation<D>) -> bool;

#[derive(Clone)]
pub struct Interpretation<D: Domain> {
    functions: InterpretationF<D>,
    relations: InterpretationR<D>,
}

pub type Valuation<Domain> = HashMap<Var, Domain>;

pub type InterpretationBool = Interpretation<bool>;

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

pub fn eval_relation<D: Domain>(
    relation: &Relation,
    interpretation: &Interpretation<D>,
    valuation: &Valuation<D>,
) -> bool {
    (interpretation.relations)(relation, interpretation.functions, valuation)
}

impl Relation {
    pub fn eval<D: Domain>(
        &self,
        interpretation: &Interpretation<D>,
        valuation: &Valuation<D>,
    ) -> bool {
        (interpretation.relations)(self, interpretation.functions, valuation)
    }
}

pub fn eval<D: Domain>(
    formula: &FirstOrderFormula,
    interpretation: &Interpretation<D>,
    valuation: &Valuation<D>,
) -> bool {
    match formula {
        Formula::True => true,

        Formula::False => false,

        Formula::Atom(atom) => eval_relation(atom, interpretation, valuation),

        Formula::Unary { op, expr } => match op {
            OpUnary::Not => !eval(expr, interpretation, valuation),
        },

        Formula::Binary { op, lhs, rhs } => match op {
            OpBinary::And => {
                lhs.eval(interpretation, valuation) && rhs.eval(interpretation, valuation)
            }
            OpBinary::Or => {
                lhs.eval(interpretation, valuation) || rhs.eval(interpretation, valuation)
            }
            OpBinary::Imp => {
                !lhs.eval(interpretation, valuation) || rhs.eval(interpretation, valuation)
            }
            OpBinary::Iff => {
                lhs.eval(interpretation, valuation) == rhs.eval(interpretation, valuation)
            }
        },

        Formula::Quantifier { .. } => todo!(),
    }
}

impl FirstOrderFormula {
    pub fn eval<D: Domain>(
        &self,
        interpretation: &Interpretation<D>,
        valuation: &Valuation<D>,
    ) -> bool {
        eval(self, interpretation, valuation)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::logic::first_order::{
            parse, terms::{Fun, Var}, Interpretation, InterpretationF, Relation, Valuation
        };

    fn interpretation_bool_functions(fun: &Fun, valuation: &Valuation<bool>) -> bool {
        match (fun.id(), fun.args()) {
            ("0", []) => false,
            ("1", []) => true,
            ("add", [x, y]) => {
                let x_val = x.eval(interpretation_bool_functions, valuation);
                let y_val = y.eval(interpretation_bool_functions, valuation);
                x_val || y_val
            }
            ("mul", [x, y]) => {
                let x_val = x.eval(interpretation_bool_functions, valuation);
                let y_val = y.eval(interpretation_bool_functions, valuation);
                x_val && y_val
            }

            _ => todo!(),
        }
    }

    fn interpretation_bool_relations(
        rel: &Relation,
        interpretation: InterpretationF<bool>,
        valuation: &Valuation<bool>,
    ) -> bool {
        match (rel.id(), rel.terms()) {
            ("eq", [x, y]) => {
                let x_val = x.eval(interpretation, valuation);
                let y_val = y.eval(interpretation, valuation);
                x_val == y_val
            }

            _ => todo!(),
        }
    }

    #[test]
    fn valuation_basic() {
        let interpretation_bool = Interpretation {
            functions: interpretation_bool_functions,
            relations: interpretation_bool_relations,
        };

        let mut valuation = HashMap::from([(Var::from("x"), true), (Var::from("y"), false)]);
        let _ = valuation.insert(Var::from("y"), true);

        let expr = parse("eq(mul(x,y), add(x, y))");
        let v = expr.eval(&interpretation_bool, &valuation);
        println!("{expr} {v}");

        // let bool_interpretation = InterpretationBool::default();
    }
}
