use std::collections::HashSet;

use crate::logic::{Atomic, Formula};

impl<A: Atomic> Formula<A> {
    pub fn free_variables(&self) -> HashSet<A::Quantum> {
        free_variables(self)
    }
}

fn free_variables<A: Atomic>(formula: &Formula<A>) -> HashSet<A::Quantum> {
    match formula {
        Formula::True | Formula::False => HashSet::default(),
        Formula::Atom(relation) => {
            let mut free = HashSet::default();

            for var in relation.variables() {
                free.insert(var.to_owned());
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
