use std::collections::{HashMap, HashSet};

use crate::logic::propositional::{Prop, PropFormula};

#[derive(Clone, Default, Debug)]
pub struct Valuation {
    assignment: Vec<(Prop, bool)>,
    map: HashMap<Prop, usize>,
}

impl Valuation {
    pub fn from_props<const N: usize>(props: [Prop; N]) -> Self {
        let mut valuation = Valuation::default();
        for prop in props {
            valuation.extend(prop, false);
        }
        valuation
    }

    pub fn from_names<const N: usize>(names: [&str; N]) -> Self {
        let mut valuation = Valuation::default();
        for name in names {
            valuation.extend(Prop::from(name), false);
        }
        valuation
    }

    pub fn from_prop_set(props: HashSet<Prop>) -> Self {
        let mut valuation = Valuation::default();
        for prop in props {
            valuation.extend(prop, false);
        }
        valuation
    }
}

impl Valuation {
    pub fn extend(&mut self, prop: Prop, val: bool) {
        self.map.insert(prop.clone(), self.assignment.len());
        self.assignment.push((prop.clone(), val));
    }

    pub fn get(&self, prop: &Prop) -> bool {
        self.assignment[self.map[prop]].1
    }

    pub fn set(&mut self, prop: &Prop, value: bool) {
        self.assignment[self.map[prop]].1 = value
    }

    // Views the valuation as a base two int and increments by one.
    //
    // Strictly, as a reversed base two int, as the bits are bumped from the left.
    pub fn next_permutation_mut(&mut self) {
        for (_, val) in self.assignment.iter_mut() {
            match val {
                true => *val = false,
                false => {
                    *val = true;
                    break;
                }
            }
        }
    }

    pub fn size(&self) -> usize {
        self.assignment.len()
    }

    pub fn permutation_count(&self) -> usize {
        2_usize.pow(
            self.size()
                .try_into()
                .expect("Permutation count exceeds usize"),
        )
    }

    pub fn assignment(&self) -> &[(Prop, bool)] {
        &self.assignment
    }

    pub fn map(&self) -> &HashMap<Prop, usize> {
        &self.map
    }

    pub fn invert(&mut self) {
        for (_, value) in self.assignment.iter_mut() {
            *value = !*value;
        }
    }

    pub fn inverted(&self) -> Valuation {
        let mut inverted = self.clone();
        inverted.invert();
        inverted
    }

    pub fn as_formula(&self) -> PropFormula {
        // In reverse so the constructed formula matches an equivalent parse without parens.
        let mut assignment = self.assignment.iter().rev();

        // An initial match to avoid top as the first conjunct.
        match assignment.next() {
            None => PropFormula::True,

            Some((prop, val)) => {
                let atom = PropFormula::Atom(prop.clone());

                let literal = match val {
                    true => atom,
                    false => PropFormula::Not(atom),
                };

                let mut formula = literal;

                #[allow(clippy::while_let_on_iterator)]
                while let Some((prop, val)) = assignment.next() {
                    let atom = PropFormula::Atom(prop.clone());
                    let literal = match val {
                        true => atom,
                        false => PropFormula::Not(atom),
                    };

                    formula = PropFormula::And(literal, formula)
                }

                formula
            }
        }
    }
}
