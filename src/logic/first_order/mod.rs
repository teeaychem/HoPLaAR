mod domains;
use std::collections::HashSet;

pub use domains::Element;

mod semantics;
pub use semantics::{Model, Valuation, eval_first_order, eval_relation, eval_term};

pub mod syntax;

pub mod terms;

pub use terms::{Term, TermId};

mod relations;
pub use relations::Relation;

pub use crate::logic::parse::parse_first_order as parse;

use crate::logic::{Formula, first_order::terms::Var};

pub type FirstOrderFormula = Formula<Relation>;

impl FirstOrderFormula {
    #[allow(non_snake_case)]
    pub fn eval<E: Element, M: Model<E>>(&self, M: &M, v: &mut Valuation<E>) -> bool {
        eval_first_order(self, M, v)
    }

    pub fn variables(&self) -> HashSet<Var> {
        let mut vars: HashSet<Var> = HashSet::default();

        for atom in self.atoms_dfs() {
            for term in &atom.terms {
                for term in term.terms_d() {
                    if let Term::V(var) = term {
                        vars.insert(var.to_owned());
                    }
                }
            }
        }

        vars
    }

    pub fn free_variables(&self) -> HashSet<Var> {
        free_variables(self)
    }
}

fn free_variables(formula: &FirstOrderFormula) -> HashSet<Var> {
    match formula {
        Formula::True | Formula::False => HashSet::default(),
        Formula::Atom(relation) => {
            let mut free = HashSet::default();

            for term in &relation.terms {
                for subterm in term.terms_d() {
                    if let Term::V(var) = subterm {
                        free.insert(var.to_owned());
                    }
                }
            }

            free
        }
        Formula::Unary { expr, .. } => free_variables(expr),
        Formula::Binary { lhs, rhs, .. } => free_variables(lhs)
            .into_iter()
            .chain(free_variables(rhs))
            .collect(),
        Formula::Quantified { var, fm: expr, .. } => {
            let mut free = free_variables(expr);
            free.remove(var);
            free
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::first_order::{parse, terms::Var};

    #[test]
    fn variables() {
        let expr = parse("forall x. (~eq(x, 0) => exists y. eq(mul(x,y), 1)))");
        let var_set = HashSet::from([Var::from("x"), Var::from("y")]);

        assert_eq!(expr.variables(), var_set);

        assert!(expr.free_variables().is_empty());

        let expr = parse("forall x. (~eq(x, 0) => eq(mul(x,y), 1))");
        assert_eq!(expr.variables(), var_set);

        assert_eq!(HashSet::from([Var::from("y")]), expr.free_variables());
    }
}
