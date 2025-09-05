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
