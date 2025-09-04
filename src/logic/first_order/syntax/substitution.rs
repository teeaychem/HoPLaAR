use std::collections::HashMap;

use crate::logic::{
    Formula,
    first_order::{
        FirstOrderFormula, Relation, Term,
        terms::{Fun, Var},
    },
};

/// Substitution, which support for ad-hoc modifications.
pub struct Substitution<S: Fn(Term) -> Term> {
    /// The substitution, an arbitrary function from terms to terms.
    function: S,
    /// Support for ad-hoc modifications.
    /// Specifically, if an key for `v` is present in `interrupt` then cases based on the value:
    /// - None, then no substitution takes place, even if given by `substitution`.
    /// - Some(alternative), then alternative is used in place of any directive from `substitution`.
    interrupt: HashMap<Var, Option<Term>>,
}

impl<S: Fn(Term) -> Term> Substitution<S> {
    pub fn from_function(fun: S) -> Self {
        Self {
            function: fun,
            interrupt: HashMap::default(),
        }
    }

    /// Adds an interrupt to the substitution and returns the existing interrupt.
    pub fn add_interrupt(&mut self, var: &Var, v: Option<Term>) -> Option<Option<Term>> {
        self.interrupt.insert(var.clone(), v)
    }

    /// Applies the substitution, ignoring any interrupts.
    pub fn apply_function(&self, key: Term) -> Term {
        match key {
            Term::F(Fun { id, args }) => {
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
            Term::F(Fun { id, args }) => {
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
    fn term_substitution<S: Fn(Term) -> Term>(
        self,
        substitution: &mut Substitution<S>,
    ) -> FirstOrderFormula {
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

            Formula::Quantified { q, var, expr } => {
                let mut fv = expr.free_variables();
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
                        let free_expr = expr.clone().term_substitution(substitution);
                        if let Some(shadowed) = out {
                            substitution.add_interrupt(&var, shadowed);
                        }

                        let free_fv = free_expr.free_variables();
                        var.variant(&free_fv)
                    }

                    false => var.clone(),
                };

                // fresh variable substitution

                let out = substitution.add_interrupt(&var, Some(Term::V(fresh_var.clone())));
                let expr = expr.term_substitution(substitution);
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

    use crate::logic::first_order::{Term, parse, syntax::substitution::Substitution, terms::Var};

    #[test]
    fn substitution() {
        let var = Term::V(Var::from("y"));

        let substitution_function = |t: Term| -> Term {
            match &t {
                y if y == &var => Term::V(Var::from("x")),
                _ => t,
            }
        };

        let mut substitution = Substitution::from_function(substitution_function);

        let expr = parse("forall x. eq(x, y)");
        let expr = expr.term_substitution(&mut substitution);

        let expected = parse("forall x'. eq(x', x)");
        assert_eq!(expr, expected);

        let expr = parse("forall x. forall x'. (eq(x, y) => eq(x,x'))");
        let expr = expr.term_substitution(&mut substitution);

        let expected = parse("forall x'. forall x''. (eq(x', x) => eq(x',x''))");
        assert_eq!(expr, expected);
    }
}
