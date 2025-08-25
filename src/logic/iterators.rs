use std::collections::VecDeque;

use crate::logic::Formula;

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

pub struct AtomIteratorB<'a, T: std::fmt::Display + std::fmt::Debug + Clone> {
    stack: VecDeque<&'a Formula<T>>,
    expr: Option<&'a Formula<T>>,
}

impl<'a, T: std::fmt::Display + std::fmt::Debug + Clone> Iterator for AtomIteratorB<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.expr {
            Some(Formula::True) | Some(Formula::False) => {
                self.expr = self.stack.pop_front();
                self.next()
            }

            Some(Formula::Atom { var }) => {
                self.expr = self.stack.pop_front();
                Some(var)
            }

            Some(Formula::OpUnary { expr, .. }) => match &**expr {
                Formula::Atom { var } => {
                    self.expr = self.stack.pop_front();
                    Some(var)
                }
                _ => {
                    self.expr = Some(expr);
                    self.next()
                }
            },

            Some(Formula::OpBinary { lhs, rhs, .. }) => match (&**lhs, &**rhs) {
                (Formula::Atom { var }, _) => {
                    self.expr = Some(rhs);
                    Some(var)
                }

                (_, Formula::Atom { var }) => {
                    self.expr = Some(lhs);
                    Some(var)
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

impl<T: std::fmt::Display + std::fmt::Debug + Clone> Formula<T> {
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
    use crate::logic::{parsing::parse_propositional_formula, propositional::Prop};

    #[test]
    fn iter_d() {
        let expr = parse_propositional_formula("(a | (c & d)) & b");
        let atoms = expr.atoms_d().cloned().collect::<Vec<_>>();
        let expected_atoms = vec![
            Prop::new("a"),
            Prop::new("c"),
            Prop::new("d"),
            Prop::new("b"),
        ];
        assert_eq!(atoms, expected_atoms);
    }

    #[test]
    fn iter_b() {
        let expr = parse_propositional_formula("((a => d) | c) & b");
        let atoms = expr.atoms_b().cloned().collect::<Vec<_>>();
        let expected_atoms = vec![
            Prop::new("b"),
            Prop::new("c"),
            Prop::new("a"),
            Prop::new("d"),
        ];
        assert_eq!(atoms, expected_atoms);
    }
}
