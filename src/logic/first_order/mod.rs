mod domains;
pub use domains::Element;

pub mod terms;

use std::collections::HashMap;

pub use terms::{Term, TermId};

mod relations;
pub use relations::Relation;

use crate::logic::{
    Formula, OpBinary, OpUnary, Quantifier,
    first_order::{
        domains::Domain,
        terms::{Fun, Var},
    },
};

pub use crate::logic::parse::parse_first_order as parse;

pub type FirstOrderFormula = Formula<Relation>;

pub type InterpretationF<D> = fn(&Fun, &Valuation<D>) -> D;
pub type InterpretationR<D> = fn(&Relation, InterpretationF<D>, &Valuation<D>) -> bool;

#[derive(Clone)]
pub struct Interpretation<E: Element> {
    domain: Domain<E>,
    functions: InterpretationF<E>,
    relations: InterpretationR<E>,
}

pub type Valuation<Domain> = HashMap<Var, Domain>;

pub type InterpretationBool = Interpretation<bool>;

impl<E: Element> Interpretation<E> {
    pub fn term_value(&self, term: &Term, valuation: &Valuation<E>) -> E {
        match term {
            Term::F(fun) => (self.functions)(fun, valuation),
            Term::V(var) => valuation[var].clone(),
        }
    }
}

impl Term {
    pub fn eval<E: Element>(
        &self,
        interpretation: InterpretationF<E>,
        valuation: &Valuation<E>,
    ) -> E {
        match self {
            Term::F(fun) => interpretation(fun, valuation),
            Term::V(var) => valuation[var].clone(),
        }
    }
}

pub fn eval_relation<E: Element>(
    relation: &Relation,
    interpretation: &Interpretation<E>,
    valuation: &Valuation<E>,
) -> bool {
    (interpretation.relations)(relation, interpretation.functions, valuation)
}

impl Relation {
    pub fn eval<E: Element>(
        &self,
        interpretation: &Interpretation<E>,
        valuation: &Valuation<E>,
    ) -> bool {
        (interpretation.relations)(self, interpretation.functions, valuation)
    }
}

pub fn eval<E: Element>(
    formula: &FirstOrderFormula,
    interpretation: &Interpretation<E>,
    valuation: &mut Valuation<E>,
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

        Formula::Quantifier { q, var, expr } => {
            let shadowed_value = valuation.get(var).cloned();
            let mut value = match q {
                Quantifier::ForAll => true,
                Quantifier::Exists => false,
            };

            for element in interpretation.domain.elements() {
                valuation.insert(var.clone(), element.clone());
                if expr.eval(interpretation, valuation) != value {
                    value = !value;
                    break;
                }
            }

            match shadowed_value {
                Some(val) => valuation.insert(var.clone(), val.clone()),
                None => valuation.remove(var),
            };

            value
        }
    }
}

impl FirstOrderFormula {
    pub fn eval<D: Element>(
        &self,
        interpretation: &Interpretation<D>,
        valuation: &mut Valuation<D>,
    ) -> bool {
        eval(self, interpretation, valuation)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::logic::first_order::{
        Interpretation, InterpretationF, Relation, Valuation,
        domains::Domain,
        parse,
        terms::{Fun, Var},
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

            _ => todo!("Request for term: {}", fun.id()),
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
            ("is_true", [x]) => x.eval(interpretation, valuation),

            _ => todo!("Request for relation: {}", rel.id()),
        }
    }

    #[test]
    fn valuation_basic() {
        let interpretation_bool = Interpretation {
            domain: Domain::from(&[true, false]),
            functions: interpretation_bool_functions,
            relations: interpretation_bool_relations,
        };

        let mut valuation = HashMap::from([(Var::from("x"), true), (Var::from("y"), false)]);
        let _ = valuation.insert(Var::from("y"), true);

        let expr = parse("exists a is_true(a)");
        let v = expr.eval(&interpretation_bool, &mut valuation);
        println!("{expr} {v}");

        let expr = parse("forall a. exists b. (eq(mul(a, a), add(a,b)))");
        let v = expr.eval(&interpretation_bool, &mut valuation);
        println!("{expr} {v}");

        // let bool_interpretation = InterpretationBool::default();
    }
}
