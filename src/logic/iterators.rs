use std::collections::VecDeque;

use crate::logic::{Atomic, Formula};

pub struct AtomIteratorD<'a, T: Atomic> {
    stack: Vec<&'a Formula<T>>,
    expr: Option<&'a Formula<T>>,
}

impl<'a, A: Atomic> Iterator for AtomIteratorD<'a, A> {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        match self.expr {
            Some(Formula::True) | Some(Formula::False) => {
                self.expr = self.stack.pop();
                self.next()
            }

            Some(Formula::Atom(atom)) => {
                self.expr = self.stack.pop();
                Some(atom)
            }

            Some(Formula::Unary { expr, .. }) => {
                self.expr = Some(expr);
                self.next()
            }

            Some(Formula::Binary { lhs, rhs, .. }) => {
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

pub struct AtomIteratorB<'a, A: Atomic> {
    stack: VecDeque<&'a Formula<A>>,
    expr: Option<&'a Formula<A>>,
}

impl<'a, A: Atomic> Iterator for AtomIteratorB<'a, A> {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        match self.expr {
            Some(Formula::True) | Some(Formula::False) => {
                self.expr = self.stack.pop_front();
                self.next()
            }

            Some(Formula::Atom(atom)) => {
                self.expr = self.stack.pop_front();
                Some(atom)
            }

            Some(Formula::Unary { expr, .. }) => match &**expr {
                Formula::Atom(atom) => {
                    self.expr = self.stack.pop_front();
                    Some(atom)
                }
                _ => {
                    self.expr = Some(expr);
                    self.next()
                }
            },

            Some(Formula::Binary { lhs, rhs, .. }) => match (&**lhs, &**rhs) {
                (Formula::Atom(atom), _) => {
                    self.expr = Some(rhs);
                    Some(atom)
                }

                (_, Formula::Atom(atom)) => {
                    self.expr = Some(lhs);
                    Some(atom)
                }

                _ => {
                    self.stack.push_back(lhs);
                    self.stack.push_back(rhs);
                    self.expr = self.stack.pop_front();
                    self.next()
                }
            },

            Some(Formula::Quantifier { expr, .. }) => {
                self.expr = Some(expr);
                self.next()
            }

            None => None,
        }
    }
}

impl<T: Atomic> Formula<T> {
    pub fn atoms_d(&'_ self) -> AtomIteratorD<'_, T> {
        AtomIteratorD {
            stack: Vec::default(),
            expr: Some(self),
        }
    }

    pub fn atoms_b(&'_ self) -> AtomIteratorB<'_, T> {
        AtomIteratorB {
            stack: VecDeque::default(),
            expr: Some(self),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{parse::parse_propositional, propositional::Prop};

    #[test]
    fn iter_d() {
        let expr = parse_propositional("(a | (c & d)) & b");
        let atoms = expr.atoms_d().cloned().collect::<Vec<_>>();
        let expected_atoms = vec![
            Prop::from("a"),
            Prop::from("c"),
            Prop::from("d"),
            Prop::from("b"),
        ];
        assert_eq!(atoms, expected_atoms);
    }

    #[test]
    fn iter_b() {
        let expr = parse_propositional("((a => d) | c) & b");
        let atoms = expr.atoms_b().cloned().collect::<Vec<_>>();
        let expected_atoms = vec![
            Prop::from("b"),
            Prop::from("c"),
            Prop::from("a"),
            Prop::from("d"),
        ];
        assert_eq!(atoms, expected_atoms);
    }
}
