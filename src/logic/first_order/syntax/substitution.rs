use std::collections::HashMap;

use crate::logic::{
    Formula,
    first_order::{FirstOrderFormula, Relation, Term, terms::Var},
};

impl FirstOrderFormula {
    pub fn term_substitution<S: Fn(Term) -> Term>(self, substitution: &S) -> FirstOrderFormula {
        let mut check = HashMap::default();
        self.term_substitution_local(substitution, &mut check)
    }

    /// The `check` argument offers ad-hoc modification of `substitution`.
    /// See `substitute_gently`.
    fn term_substitution_local<S: Fn(Term) -> Term>(
        self,
        substitution: &S,
        check: &mut HashMap<Var, Option<Term>>,
    ) -> FirstOrderFormula {
        match self {
            Formula::True | Formula::False => self,

            Formula::Atom(Relation { id, terms }) => {
                let fresh = Relation::from(
                    id,
                    terms
                        .into_iter()
                        .map(|t| t.substitute_gently(substitution, check))
                        .collect(),
                );
                Formula::Atom(fresh)
            }

            Formula::Unary { op, expr } => {
                Formula::Unary(op, expr.term_substitution_local(substitution, check))
            }

            Formula::Binary { op, lhs, rhs } => Formula::Binary(
                op,
                lhs.term_substitution_local(substitution, check),
                rhs.term_substitution_local(substitution, check),
            ),

            Formula::Quantifier { q, var, expr } => {
                let mut fv = expr.free_variables();
                fv.remove(&var);

                let fresh_bind = fv.iter().any(|y| {
                    Term::V(y.clone())
                        .substitute_gently(substitution, check)
                        .variables()
                        .contains(&var)
                });

                let fresh_var = match fresh_bind {
                    true => {
                        // variable free substitution

                        let out = check.insert(var.clone(), None);
                        let free_expr = expr.clone().term_substitution_local(substitution, check);
                        if let Some(shadowed) = out {
                            check.insert(var.clone(), shadowed);
                        }

                        let free_fv = free_expr.free_variables();
                        var.variant(&free_fv)
                    }

                    false => var.clone(),
                };

                // fresh variable substitution

                let out = check.insert(var.clone(), Some(Term::V(fresh_var.clone())));
                let expr = expr.term_substitution_local(substitution, check);
                if let Some(shadowed) = out {
                    check.insert(var.clone(), shadowed);
                }

                Formula::Quantifier(q, fresh_var, expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::first_order::{Term, parse, terms::Var};

    #[test]
    fn substitution() {
        let var = Term::V(Var::from("y"));
        let substitution = |t: Term| -> Term {
            match &t {
                y if y == &var => Term::V(Var::from("x")),
                _ => t,
            }
        };

        let expr = parse("forall x. eq(x, y)");
        let expr = expr.term_substitution(&substitution);

        let expected = parse("forall x'. eq(x', x)");
        assert_eq!(expr, expected);

        let expr = parse("forall x. forall x'. (eq(x, y) => eq(x,x'))");
        let expr = expr.term_substitution(&substitution);

        let expected = parse("forall x'. forall x''. (eq(x', x) => eq(x',x''))");
        assert_eq!(expr, expected);
    }
}
