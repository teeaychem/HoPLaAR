mod model;
pub use model::Model;

pub mod models;

mod valuation;
pub use valuation::Valuation;

use crate::logic::{
    Formula, OpBinary, OpUnary, Quantifier,
    first_order::{Element, FirstOrderFormula, Relation, Term},
};

#[allow(non_snake_case)]
pub fn eval_term<E: Element, M: Model<E>>(term: &Term, M: &M, v: &Valuation<E>) -> E {
    match term {
        Term::F(fun) => M.functions(fun, v),
        Term::V(var) => v.get(var).unwrap().clone(),
    }
}

#[allow(non_snake_case)]
pub fn eval_relation<E: Element, M: Model<E>>(
    relation: &Relation,
    M: &M,
    v: &Valuation<E>,
) -> bool {
    M.relations(relation, v)
}

#[allow(non_snake_case)]
pub fn eval_first_order<E: Element, M: Model<E>>(
    formula: &FirstOrderFormula,
    M: &M,
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

        Formula::Quantified { q, var, fm: expr } => {
            let shadowed_value = v.get(var).cloned();
            let mut value = match q {
                Quantifier::ForAll => true,
                Quantifier::Exists => false,
            };

            for element in M.elements() {
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
    use crate::logic::first_order::{Valuation, domains::Domain, parse};

    #[test]
    fn scope() {
        let m = Domain::boolean();
        let mut v = Valuation::undefined();

        let narrow = parse("forall x. eq(x, 0) => eq(1,0)");
        let wide = parse("forall x. (eq(x, 0) => eq(1,0))");

        assert!(narrow.eval(&m, &mut v));
        assert!(!wide.eval(&m, &mut v));
    }
}
