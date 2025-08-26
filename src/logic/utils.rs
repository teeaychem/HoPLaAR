use std::collections::HashSet;

use crate::logic::{Atomic, Formula};

pub fn on_atoms<F, A>(f: &F, fm: &Formula<A>) -> Formula<A>
where
    F: Fn(&Formula<A>) -> Formula<A>,
    A: Atomic,
{
    fm.on_atoms(f)
}

impl<A: Atomic> Formula<A> {
    pub fn on_atoms<F: Fn(&Formula<A>) -> Formula<A>>(&self, f: &F) -> Formula<A> {
        match self {
            Formula::True => Formula::True,

            Formula::False => Formula::False,

            Formula::Atom { .. } => f(self),

            Formula::OpUnary { op, expr } => Formula::Unary(*op, expr.on_atoms(f)),

            Formula::OpBinary { op, lhs, rhs } => {
                Formula::Binary(*op, lhs.on_atoms(f), rhs.on_atoms(f))
            }

            Formula::Quantifier { q, var, expr } => {
                Formula::Quantifier(*q, var.clone(), expr.on_atoms(f))
            }
        }
    }

    pub fn on_atoms_mut<F: Fn(&mut Formula<A>)>(&mut self, f: &F) {
        match self {
            Formula::True | Formula::False => {}

            Formula::Atom { .. } => f(self),

            Formula::OpUnary { expr, .. } => {
                expr.on_atoms_mut(f);
            }

            Formula::OpBinary { lhs, rhs, .. } => {
                lhs.on_atoms_mut(f);
                rhs.on_atoms_mut(f);
            }

            Formula::Quantifier { expr, .. } => {
                expr.on_atoms_mut(f);
            }
        }
    }

    pub fn atoms(&self) -> HashSet<A> {
        self.atoms_d().cloned().collect()
    }

    pub fn substitute(&self, atom: &A, formula: &Formula<A>) -> Formula<A> {
        match self {
            Formula::True | Formula::False => self.clone(),
            Formula::Atom { var } => {
                if var == atom {
                    formula.to_owned()
                } else {
                    self.clone()
                }
            }
            Formula::OpUnary { op, expr } => Formula::Unary(*op, expr.substitute(atom, formula)),
            Formula::OpBinary { op, lhs, rhs } => Formula::Binary(
                *op,
                lhs.substitute(atom, formula),
                rhs.substitute(atom, formula),
            ),
            Formula::Quantifier { q, var, expr } => {
                Formula::Quantifier(*q, var.to_owned(), expr.substitute(atom, formula))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::{
        Formula,
        parsing::parse_propositional_formula,
        propositional::{Prop, PropFormula},
        utils::on_atoms,
    };

    #[test]
    fn all_true() {
        let mut expr = PropFormula::And(
            PropFormula::Atom(Prop::from("p")),
            PropFormula::Atom(Prop::from("q")),
        );
        let atom_true = |_: &PropFormula| PropFormula::True;
        expr = on_atoms(&atom_true, &expr);
        assert_eq!(expr, PropFormula::And(PropFormula::True, PropFormula::True))
    }

    #[test]
    fn uppercase() {
        let mut expr_lower = PropFormula::And(
            PropFormula::Atom(Prop::from("p")),
            PropFormula::Atom(Prop::from("q")),
        );

        let expr_upper = PropFormula::And(
            PropFormula::Atom(Prop::from("P")),
            PropFormula::Atom(Prop::from("Q")),
        );

        let uppercase_pure = |fm: &PropFormula| -> Formula<Prop> {
            if let Formula::Atom { var } = &fm {
                let prop = Prop::from(var.name().to_uppercase().as_str());
                Formula::Atom(prop)
            } else {
                fm.clone()
            }
        };

        let expr_upper_pure = on_atoms(&uppercase_pure, &expr_lower);
        assert_eq!(expr_upper_pure, expr_upper);

        let uppercase_mut = |fm: &mut PropFormula| {
            if let Formula::Atom { var } = fm {
                let prop = Prop::from(var.name().to_uppercase().as_str());
                *var = prop;
            }
        };

        expr_lower.on_atoms_mut(&uppercase_mut);
        assert_eq!(expr_lower, expr_upper);
    }

    #[test]
    fn union() {
        let expr = parse_propositional_formula("(a | (c & d)) & b");
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
        let expr = parse_propositional_formula("p & q & p & q");
        let s = parse_propositional_formula("p & q");
        let expr_s = expr.substitute(&Prop::from("p"), &s);

        let expected = parse_propositional_formula("(p & q) & q & (p & q) & q");

        assert_eq!(expected, expr_s);
    }
}
