use std::collections::{HashMap, HashSet};

use crate::logic::{
    Literal,
    propositional::{Prop, PropFormula},
};

#[derive(Clone, Default, Debug)]
pub struct Valuation {
    fixed: Vec<bool>,
    assignment: Vec<Literal<Prop>>,
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

    pub fn from_ids<const N: usize>(ids: [&str; N]) -> Self {
        let mut valuation = Valuation::default();
        for id in ids {
            valuation.extend(Prop::from(id), false);
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
    pub fn get(&self, prop: &Prop) -> bool {
        self.assignment[self.map[prop]].value()
    }

    pub fn size(&self) -> usize {
        self.assignment.len()
    }

    pub fn free_atom_count(&self) -> usize {
        self.fixed.iter().filter(|f| !**f).count()
    }

    pub fn permutation_count(&self) -> usize {
        2_usize.pow(
            self.free_atom_count()
                .try_into()
                .expect("Valuation free count exceeds usize"),
        )
    }

    pub fn assignment(&self) -> &[Literal<Prop>] {
        &self.assignment
    }

    pub fn map(&self) -> &HashMap<Prop, usize> {
        &self.map
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

            Some(literal) => {
                let atom = PropFormula::Atom(literal.atom().clone());

                let literal = match literal.value() {
                    true => atom,
                    false => PropFormula::Not(atom),
                };

                let mut formula = literal;

                #[allow(clippy::while_let_on_iterator)]
                while let Some(literal) = assignment.next() {
                    let atom = PropFormula::Atom(literal.atom().clone());
                    let literal = match literal.value() {
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

impl Valuation {
    pub fn set(&mut self, prop: &Prop, value: bool) {
        self.assignment[self.map[prop]].set_value(value)
    }

    pub fn extend(&mut self, prop: Prop, val: bool) {
        self.map.insert(prop.clone(), self.assignment.len());
        self.assignment.push(Literal::from(prop.clone(), val));
        self.fixed.push(false);
    }

    pub fn fix(&mut self, prop: &Prop) {
        self.fixed[self.map[prop]] = true
    }

    // Views the valuation as a base two int and increments by one.
    //
    // Strictly, as a reversed base two int, as the bits are bumped from the left.
    pub fn next_permutation_mut(&mut self) {
        for (idx, literal) in self.assignment.iter_mut().enumerate() {
            if !self.fixed[idx] {
                match literal.value() {
                    true => literal.set_value(false),
                    false => {
                        literal.set_value(true);
                        break;
                    }
                }
            }
        }
    }

    pub fn invert(&mut self) {
        for (idx, literal) in self.assignment.iter_mut().enumerate() {
            if !self.fixed[idx] {
                literal.invert_value();
            }
        }
    }
}

impl PropFormula {
    pub fn all_sat_valuations(&self) -> Vec<Valuation> {
        let mut sat_valuations = Vec::default();

        let mut valuation = Valuation::from_prop_set(self.atoms());
        for _ in 0..valuation.permutation_count() {
            if self.eval(&valuation) {
                sat_valuations.push(valuation.clone());
            }
            valuation.next_permutation_mut();
        }

        sat_valuations
    }

    pub fn all_unsat_valuations(&self) -> Vec<Valuation> {
        let mut unsat_valuations = Vec::default();

        let mut valuation = Valuation::from_prop_set(self.atoms());
        for _ in 0..valuation.permutation_count() {
            if !self.eval(&valuation) {
                unsat_valuations.push(valuation.clone());
            }
            valuation.next_permutation_mut();
        }

        unsat_valuations
    }

    pub fn all_sat_valuations_from(&self, mut valuation: Valuation) -> Vec<Valuation> {
        let mut sat_valuations = Vec::default();

        for _ in 0..2_usize.pow(
            valuation
                .free_atom_count()
                .try_into()
                .expect("Free atoms exceed u32"),
        ) {
            if self.eval(&valuation) {
                sat_valuations.push(valuation.clone());
            }
            valuation.next_permutation_mut();
        }

        sat_valuations
    }

    pub fn all_unsat_valuations_from(&self, mut valuation: Valuation) -> Vec<Valuation> {
        let mut unsat_valuations = Vec::default();

        for _ in 0..2_usize.pow(
            valuation
                .free_atom_count()
                .try_into()
                .expect("Free atoms exceed u32"),
        ) {
            if !self.eval(&valuation) {
                unsat_valuations.push(valuation.clone());
            }
            valuation.next_permutation_mut();
        }

        unsat_valuations
    }
}

impl std::fmt::Display for Valuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for literal in &self.assignment {
            // Consistent spacing of literals.
            let _ = match literal.value() {
                true => write!(f, " {literal} "),
                false => write!(f, "{literal} "),
            };
        }
        Ok(())
    }
}
