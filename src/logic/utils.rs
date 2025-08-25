use crate::logic::{
    Formula,
    propositional::{Prop, PropFormula},
};

pub fn on_atoms<F, T>(f: &F, fm: &Formula<T>) -> Formula<T>
where
    F: Fn(&Formula<T>) -> Formula<T>,
    T: std::fmt::Display + std::fmt::Debug + Clone,
{
    fm.on_atoms(f)
}

impl<T: std::fmt::Display + std::fmt::Debug + Clone> Formula<T> {
    pub fn on_atoms<F: Fn(&Formula<T>) -> Formula<T>>(&self, f: &F) -> Formula<T> {
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

    pub fn on_atoms_mut<F: Fn(&mut Formula<T>)>(&mut self, f: &F) {
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
}

pub struct AtomIteratorD<'a, T: std::fmt::Display + std::fmt::Debug + Clone> {
    stack: Vec<&'a Formula<T>>,
    expr: Option<&'a Formula<T>>,
}

impl<'a, T: std::fmt::Display + std::fmt::Debug + Clone> Iterator for AtomIteratorD<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.expr {
            Some(Formula::True) | Some(Formula::False) => {
                self.expr = self.stack.pop();
                self.next()
            }

            Some(Formula::Atom { var }) => {
                self.expr = self.stack.pop();
                Some(var)
            }

            Some(Formula::OpUnary { expr, .. }) => {
                self.expr = Some(expr);
                self.next()
            }

            Some(Formula::OpBinary { lhs, rhs, .. }) => {
                self.stack.push(rhs);
                self.expr = Some(lhs);
                self.next()
            }

            Some(Formula::Quantifier { expr, .. }) => {
                self.expr = Some(expr);
                self.next()
            }

            None => None,
        }
    }
}

impl<T: std::fmt::Display + std::fmt::Debug + Clone> Formula<T> {
    pub fn atoms_d(&'_ self) -> AtomIteratorD<'_, T> {
        AtomIteratorD {
            stack: Vec::default(),
            expr: Some(self),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{
        Formula,
        parsing::parse_propositional_formula,
        propositional::{Prop, PropFormula},
        utils::on_atoms,
    };

    #[test]
    fn all_true() {
        let mut expr = PropFormula::And(
            PropFormula::Atom(Prop::new("p")),
            PropFormula::Atom(Prop::new("q")),
        );
        let atom_true = |_: &PropFormula| PropFormula::True;
        expr = on_atoms(&atom_true, &expr);
        assert_eq!(expr, PropFormula::And(PropFormula::True, PropFormula::True))
    }

    #[test]
    fn uppercase() {
        let mut expr_lower = PropFormula::And(
            PropFormula::Atom(Prop::new("p")),
            PropFormula::Atom(Prop::new("q")),
        );

        let expr_upper = PropFormula::And(
            PropFormula::Atom(Prop::new("P")),
            PropFormula::Atom(Prop::new("Q")),
        );

        let uppercase_pure = |fm: &PropFormula| -> Formula<Prop> {
            if let Formula::Atom { var } = &fm {
                let prop = Prop::new(var.name().to_uppercase().as_str());
                Formula::Atom(prop)
            } else {
                fm.clone()
            }
        };

        let expr_upper_pure = on_atoms(&uppercase_pure, &expr_lower);
        assert_eq!(expr_upper_pure, expr_upper);

        let uppercase_mut = |fm: &mut PropFormula| {
            if let Formula::Atom { var } = fm {
                let prop = Prop::new(var.name().to_uppercase().as_str());
                *var = prop;
            }
        };

        expr_lower.on_atoms_mut(&uppercase_mut);
        assert_eq!(expr_lower, expr_upper);
    }

    #[test]
    fn iter_d() {
        let expr = parse_propositional_formula("(a | c) & b");
        let atoms = expr.atoms_d().cloned().collect::<Vec<_>>();
        let expected_atoms = vec![Prop::new("a"), Prop::new("c"), Prop::new("b")];
        assert_eq!(atoms, expected_atoms);
    }
}
