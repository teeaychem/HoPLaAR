use crate::logic::{
    Formula, OpBinary, OpUnary, Quantifier,
    first_order::{
        Element, FirstOrderFormula, Relation, Term,
        domains::Domain,
        terms::{Fun, Var},
    },
};

use std::collections::HashMap;

pub type InterpretationF<D> = fn(&Fun, &Valuation<D>) -> D;

pub type InterpretationR<D> = fn(&Relation, InterpretationF<D>, &Valuation<D>) -> bool;

#[derive(Clone)]
pub struct Interpretation<E: Element> {
    domain: Domain<E>,
    functions: InterpretationF<E>,
    relations: InterpretationR<E>,
}

impl<E: Element> Interpretation<E> {
    pub fn from(
        domain: Domain<E>,
        functions: InterpretationF<E>,
        relations: InterpretationR<E>,
    ) -> Self {
        Self {
            domain,
            functions,
            relations,
        }
    }
}

impl<E: Element> Interpretation<E> {
    pub fn domain(&self) -> &Domain<E> {
        &self.domain
    }

    // pub fn functions(&self) -> InterpretationF<E> {
    //     self.functions
    // }

    // pub fn relations(&self) -> InterpretationR<E> {
    //     self.relations
    // }

    pub fn interpret_function(&self, fun: &Fun, v: &Valuation<E>) -> E {
        (self.functions)(fun, v)
    }

    pub fn interpret_relation(&self, relation: &Relation, v: &Valuation<E>) -> bool {
        (self.relations)(relation, self.functions, v)
    }
}

pub type Valuation<Domain> = HashMap<Var, Domain>;

#[allow(non_snake_case)]
pub fn eval_term<E: Element>(term: &Term, M: InterpretationF<E>, v: &Valuation<E>) -> E {
    match term {
        Term::F(fun) => M(fun, v),
        Term::V(var) => v[var].clone(),
    }
}

#[allow(non_snake_case)]
pub fn eval_relation<E: Element>(
    relation: &Relation,
    M: &Interpretation<E>,
    v: &Valuation<E>,
) -> bool {
    M.interpret_relation(relation, v)
}

#[allow(non_snake_case)]
pub fn eval_first_order<E: Element>(
    formula: &FirstOrderFormula,
    M: &Interpretation<E>,
    v: &mut Valuation<E>,
) -> bool {
    match formula {
        Formula::True => true,
        Formula::False => false,
        Formula::Atom(atom) => eval_relation(atom, M, v),
        Formula::Unary { op, expr } => match op {
            OpUnary::Not => !eval_first_order(expr, M, v),
        },
        Formula::Binary { op, lhs, rhs } => match op {
            OpBinary::And => lhs.eval(M, v) && rhs.eval(M, v),
            OpBinary::Or => lhs.eval(M, v) || rhs.eval(M, v),
            OpBinary::Imp => !lhs.eval(M, v) || rhs.eval(M, v),
            OpBinary::Iff => lhs.eval(M, v) == rhs.eval(M, v),
        },

        Formula::Quantifier { q, var, expr } => {
            let shadowed_value = v.get(var).cloned();
            let mut value = match q {
                Quantifier::ForAll => true,
                Quantifier::Exists => false,
            };

            for element in M.domain.elements() {
                v.insert(var.clone(), element.clone());
                if expr.eval(M, v) != value {
                    value = !value;
                    break;
                }
            }

            match shadowed_value {
                Some(val) => v.insert(var.clone(), val.clone()),
                None => v.remove(var),
            };

            value
        }
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

    fn interpret_bool_function(fun: &Fun, v: &Valuation<bool>) -> bool {
        match (fun.id(), fun.args()) {
            ("0", []) => false,
            ("1", []) => true,
            ("add", [x, y]) => {
                let x_val = x.eval(interpret_bool_function, v);
                let y_val = y.eval(interpret_bool_function, v);
                x_val || y_val
            }
            ("mul", [x, y]) => {
                let x_val = x.eval(interpret_bool_function, v);
                let y_val = y.eval(interpret_bool_function, v);
                x_val && y_val
            }

            _ => todo!("Request for term: {}", fun.id()),
        }
    }

    #[allow(non_snake_case)]
    fn interpret_bool_relation(
        rel: &Relation,
        I: InterpretationF<bool>,
        v: &Valuation<bool>,
    ) -> bool {
        match (rel.id(), rel.terms()) {
            ("eq", [x, y]) => {
                let x_val = x.eval(I, v);
                let y_val = y.eval(I, v);
                x_val == y_val
            }
            ("is_true", [x]) => x.eval(I, v),

            _ => todo!("Request for relation: {}", rel.id()),
        }
    }

    #[test]
    fn valuation_basic() {
        let interpretation_bool = Interpretation::from(
            Domain::from(&[true, false]),
            interpret_bool_function,
            interpret_bool_relation,
        );

        let mut v = HashMap::from([(Var::from("x"), true), (Var::from("y"), false)]);
        let _ = v.insert(Var::from("y"), true);

        let expr = parse("exists a is_true(a)");
        let result = expr.eval(&interpretation_bool, &mut v);
        println!("{expr} {result}");

        let expr = parse("forall a. exists b. ~(is_true(mul(a,b)))");
        let result = expr.eval(&interpretation_bool, &mut v);
        println!("{expr} {result}");

        // let bool_interpretation = InterpretationBool::default();
    }
}
