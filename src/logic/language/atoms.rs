use std::collections::HashSet;

use crate::logic::{Atomic, Formula};

impl<A: Atomic> Formula<A> {
    pub fn on_atoms<F>(mut self, f: &F) -> Self
    where
        F: Fn(Formula<A>) -> Formula<A>,
    {
        match &mut self {
            Formula::True | Formula::False => self,

            Formula::Atom { .. } => f(self),

            Formula::Unary { op, expr } => Formula::Unary(*op, std::mem::take(expr).on_atoms(f)),

            Formula::Binary { op, lhs, rhs } => Formula::Binary(
                *op,
                std::mem::take(lhs).on_atoms(f),
                std::mem::take(rhs).on_atoms(f),
            ),

            Formula::Quantified { q, var, fm: expr } => {
                Formula::Quantified(*q, var.to_owned(), std::mem::take(expr).on_atoms(f))
            }
        }
    }

    pub fn atoms(&self) -> HashSet<A> {
        self.atoms_dfs().cloned().collect()
    }

    pub fn substitute(mut self, atom: &A, fm: &Formula<A>) -> Formula<A> {
        match &mut self {
            Formula::True | Formula::False => self,
            Formula::Atom(other) => {
                if other == atom {
                    fm.to_owned()
                } else {
                    self
                }
            }
            Formula::Unary { op, expr } => {
                Formula::Unary(*op, std::mem::take(expr).substitute(atom, fm))
            }
            Formula::Binary { op, lhs, rhs } => Formula::Binary(
                *op,
                std::mem::take(lhs).substitute(atom, fm),
                std::mem::take(rhs).substitute(atom, fm),
            ),
            Formula::Quantified { q, var, fm: expr } => Formula::Quantified(
                *q,
                var.to_owned(),
                std::mem::take(expr).substitute(atom, fm),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::{
        Atomic, Formula,
        parse::parse_propositional,
        propositional::{Prop, PropFormula, Valuation},
    };

    #[test]
    fn all_true() {
        let mut expr = PropFormula::And(
            PropFormula::Atom(Prop::from("p")),
            PropFormula::Atom(Prop::from("q")),
        );
        let atom_true = |_: PropFormula| PropFormula::True;
        expr = expr.on_atoms(&atom_true);
        assert_eq!(expr, PropFormula::And(PropFormula::True, PropFormula::True))
    }

    #[test]
    fn uppercase() {
        let expr_lower = PropFormula::And(
            PropFormula::Atom(Prop::from("p")),
            PropFormula::Atom(Prop::from("q")),
        );

        let expr_upper = PropFormula::And(
            PropFormula::Atom(Prop::from("P")),
            PropFormula::Atom(Prop::from("Q")),
        );

        let uppercase_mut = |fm: PropFormula| -> PropFormula {
            if let Formula::Atom(atom) = fm {
                Formula::Atom(Prop::from(&atom.id().to_uppercase()))
            } else {
                fm
            }
        };

        let expr_lower = expr_lower.on_atoms(&uppercase_mut);
        assert_eq!(expr_lower, expr_upper);
    }

    #[test]
    fn union() {
        let expr = parse_propositional("(a | (c & d)) & b");
        let atom_union = expr.atoms();
        let atom_set = HashSet::from([
            Prop::from("b"),
            Prop::from("c"),
            Prop::from("a"),
            Prop::from("d"),
        ]);
        assert_eq!(atom_union, atom_set);
    }

    #[test]
    fn substitution() {
        let expr = parse_propositional("p & q & p & q");
        let s = parse_propositional("p & q");
        let expr_s = expr.substitute(&Prop::from("p"), &s);

        let expected = parse_propositional("(p & q) & q & (p & q) & q");

        assert_eq!(expected, expr_s);
    }

    #[test]
    fn duals() {
        let expr = parse_propositional("p | ~ p");
        let expected = parse_propositional("p & ~ p");
        assert_eq!(expr.dual(), expected);
    }

    #[test]
    fn thm_2_7() {
        let expr = parse_propositional("p & q");
        let v_a = Valuation::from_prop_set(expr.atoms());
        assert_eq!(expr.eval(&v_a), !expr.dual().eval(&v_a.inverted()))
    }
}
