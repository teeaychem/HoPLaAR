mod domains;

pub use domains::Element;

pub mod ground;

pub mod library;

mod semantics;
pub use semantics::{Model, Valuation, eval_first_order, eval_relation, eval_term};

pub mod syntax;

pub mod terms;

pub use terms::{Term, TermId};

mod relations;
pub use relations::Relation;

mod tableaux;

mod unification;

// Local usage

use std::collections::HashSet;

use crate::logic::{
    Atomic, Formula,
    first_order::terms::{Fun, Var},
};

pub type FirstOrderFormula = Formula<Relation>;

impl FirstOrderFormula {
    #[allow(non_snake_case)]
    pub fn eval<E: Element, M: Model<E>>(&self, M: &M, v: &mut Valuation<E>) -> bool {
        eval_first_order(self, M, v)
    }

    pub fn constants(&self) -> HashSet<Fun> {
        let mut constants: HashSet<Fun> = HashSet::default();

        for atom in self.atoms_dfs() {
            for function in atom.functions().filter(|f| f.arity() == 0) {
                constants.insert(function.to_owned());
            }
        }

        constants
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

    pub fn functions(&self) -> HashSet<Fun> {
        let mut funs: HashSet<Fun> = HashSet::default();

        for atom in self.atoms_dfs() {
            for function in atom.functions() {
                funs.insert(function.to_owned());
            }
        }

        funs
    }
}

impl FirstOrderFormula {
    fn split_on_helper(mut self, split_op: super::OpBinary, dest: &mut Vec<FirstOrderFormula>) {
        match self {
            Formula::Binary {
                op,
                ref mut lhs,
                ref mut rhs,
            } => {
                //
                match op == split_op {
                    true => {
                        std::mem::take(lhs).split_on_helper(split_op, dest);
                        std::mem::take(rhs).split_on_helper(split_op, dest);
                    }
                    false => dest.push(self),
                }
            }
            _ => dest.push(self),
        }
    }

    pub fn split_on(self, split_op: super::OpBinary) -> Vec<FirstOrderFormula> {
        let mut dest: Vec<FirstOrderFormula> = Vec::default();
        self.split_on_helper(split_op, &mut dest);
        dest
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::first_order::{FirstOrderFormula, terms::Var};

    #[test]
    fn variables() {
        let expr = FirstOrderFormula::from("forall x. (~eq(x, 0) => exists y. eq(mul(x,y), 1)))");
        let var_set = HashSet::from([Var::from("x"), Var::from("y")]);

        assert_eq!(expr.variables(), var_set);

        assert!(expr.free_variables().is_empty());

        let expr = FirstOrderFormula::from("forall x. (~eq(x, 0) => eq(mul(x,y), 1))");
        assert_eq!(expr.variables(), var_set);

        assert_eq!(HashSet::from([Var::from("y")]), expr.free_variables());
    }
}
