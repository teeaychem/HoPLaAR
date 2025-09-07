use std::collections::HashMap;

use crate::logic::{
    Formula,
    first_order::{
        FirstOrderFormula, Relation, Term,
        terms::{Fun, Var},
    },
};

/// Substitution, which support for ad-hoc modifications.
pub struct Substitution {
    /// The substitution, an arbitrary function from terms to terms.
    function: Box<dyn Fn(Term) -> Term>,
    /// Support for ad-hoc modifications.
    /// Specifically, if an key for `v` is present in `interrupt` then cases based on the value:
    /// - None, then no substitution takes place, even if given by `substitution`.
    /// - Some(alternative), then alternative is used in place of any directive from `substitution`.
    interrupt: HashMap<Var, Option<Term>>,
}

impl std::default::Default for Substitution {
    fn default() -> Self {
        Self {
            function: Box::new(|t: Term| -> Term { t }),
            interrupt: HashMap::default(),
        }
    }
}

impl Substitution {
    pub fn from_function(fun: Box<dyn Fn(Term) -> Term>) -> Self {
        Self {
            function: Box::new(fun),
            interrupt: HashMap::default(),
        }
    }

    /// Adds an interrupt to the substitution and returns the existing interrupt.
    pub fn add_interrupt(&mut self, var: &Var, v: Option<Term>) -> Option<Option<Term>> {
        self.interrupt.insert(var.clone(), v)
    }

    /// Adds an interrupt to the substitution and returns the existing interrupt.
    pub fn remove_interrupt(&mut self, var: &Var) -> Option<Option<Term>> {
        self.interrupt.remove(var)
    }

    /// Applies the substitution, ignoring any interrupts.
    pub fn apply_function(&self, key: Term) -> Term {
        match key {
            Term::F(Fun { id, args, .. }) => {
                let x: Vec<Term> = args
                    .into_iter()
                    .map(|arg| self.apply_function(arg))
                    .collect();
                Term::Fun(&id, &x)
            }

            Term::V(_) => (self.function)(key),
        }
    }

    /// Applies the substitution, adhering to any interrupts.
    pub fn apply(&self, key: Term) -> Term {
        match key {
            Term::F(Fun { id, args, .. }) => {
                let x: Vec<Term> = args.into_iter().map(|arg| self.apply(arg)).collect();
                Term::Fun(&id, &x)
            }

            Term::V(ref var) => match self.interrupt.get(var) {
                Some(Some(out)) => out.clone(),
                Some(None) => key,
                None => (self.function)(key),
            },
        }
    }
}

impl FirstOrderFormula {
    pub fn term_substitution(self, substitution: &mut Substitution) -> FirstOrderFormula {
        match self {
            Formula::True | Formula::False => self,

            Formula::Atom(Relation { id, terms }) => {
                let fresh = Relation::from(
                    id,
                    terms.into_iter().map(|t| substitution.apply(t)).collect(),
                );
                Formula::Atom(fresh)
            }

            Formula::Unary { op, expr } => Formula::Unary(op, expr.term_substitution(substitution)),

            Formula::Binary { op, lhs, rhs } => Formula::Binary(
                op,
                lhs.term_substitution(substitution),
                rhs.term_substitution(substitution),
            ),

            Formula::Quantified { q, var, fm } => {
                let mut fv = fm.free_variables();
                fv.remove(&var);

                let fresh_bind = fv.iter().any(|y| {
                    substitution
                        .apply(Term::V(y.clone()))
                        .variables()
                        .contains(&var)
                });

                let fresh_var = match fresh_bind {
                    true => {
                        // variable free substitution

                        let out = substitution.add_interrupt(&var, None);
                        let free_expr = fm.clone().term_substitution(substitution);
                        if let Some(shadowed) = out {
                            substitution.add_interrupt(&var, shadowed);
                        }

                        let free_fv = free_expr.free_variables();
                        var.fresh_variant(free_fv.iter())
                    }

                    false => var.clone(),
                };

                // fresh variable substitution

                let out = substitution.add_interrupt(&var, Some(Term::V(fresh_var.clone())));
                let expr = fm.term_substitution(substitution);
                if let Some(shadowed) = out {
                    substitution.add_interrupt(&var, shadowed);
                }

                Formula::Quantified(q, fresh_var, expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::first_order::{
        FirstOrderFormula, Term, syntax::substitution::Substitution, terms::Var,
    };

    #[test]
    fn substitution() {
        let var = Term::V(Var::from("y"));

        let substitution_function = move |t: Term| -> Term {
            match &t {
                y if y == &var => Term::V(Var::from("x")),
                _ => t,
            }
        };

        let mut substitution = Substitution::from_function(Box::new(substitution_function));

        let expr = FirstOrderFormula::from("forall x. eq(x, y)");
        let expr = expr.term_substitution(&mut substitution);

        let expected = FirstOrderFormula::from("forall x_1. eq(x_1, x)");
        assert_eq!(expr, expected);

        let expr = FirstOrderFormula::from("forall x. forall x_1. (eq(x, y) => eq(x,x_1))");
        let expr = expr.term_substitution(&mut substitution);

        let expected =
            FirstOrderFormula::from("forall x_1. forall x_2. (eq(x_1, x) => eq(x_1, x_2))");

        assert_eq!(expr, expected);
    }
}
