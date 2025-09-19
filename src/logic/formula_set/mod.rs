mod cnf;
mod dnf;
mod literal_set;
use std::collections::HashMap;

pub use literal_set::LiteralSet;

mod propositional;
mod transformations;

use crate::logic::{Atomic, Formula, Literal, OpBinary};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    CNF,
    DNF,
}

// A formula, as a set of sets.
// Invariant: `formula` is sorted by `literal_set_cmp`.
#[derive(Debug, PartialEq, Eq)]
pub struct FormulaSet<A: Atomic> {
    sets: Vec<LiteralSet<A>>,
    atoms: HashMap<String, (bool, bool)>,
    mode: Mode,
}

impl<A: Atomic> FormulaSet<A> {
    fn literal_set_to_formula(op: OpBinary, ls: &LiteralSet<A>) -> Formula<A> {
        match ls.as_slice() {
            [] => Formula::True,
            [literal] => literal.as_formula(),
            [first, remaining @ ..] => {
                let mut formula = first.as_formula();
                for other in remaining {
                    formula = Formula::Binary(op, formula, other.as_formula());
                }
                formula
            }
        }
    }
}

impl<A: Atomic> std::fmt::Display for FormulaSet<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let outer_limit = self.sets.len().saturating_sub(1);

        let _ = write!(f, "{{");
        for (outer_idx, expr) in self.sets.iter().enumerate() {
            let inner_limit = expr.len().saturating_sub(1);

            let _ = write!(f, "{{");
            for (inner_idx, literal) in expr.set.iter().enumerate() {
                let _ = write!(f, "{literal}");
                if inner_idx < inner_limit {
                    let _ = write!(f, ", ");
                }
            }
            let _ = write!(f, "}}");
            if outer_idx < outer_limit {
                let _ = write!(f, ", ");
            }
        }
        let _ = write!(f, "}}");

        Ok(())
    }
}

impl<A: Atomic> Formula<A> {
    pub fn to_set_direct(&self, mode: Mode) -> FormulaSet<A> {
        let mut fs = FormulaSet::empty(mode);

        let mut sets = match mode {
            Mode::CNF => self.to_cnf_set_local(),
            Mode::DNF => self.to_dnf_set_local(),
        };
        sets.sort();
        sets.dedup();

        for literal in sets.iter().flat_map(|s| &s.set) {
            fs.note_literal(literal);
        }

        std::mem::swap(&mut fs.sets, &mut sets);

        fs
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn note_literal(&mut self, literal: &Literal<A>) {
        match literal.value() {
            true => self.atoms.entry(literal.id().to_owned()).or_default().0 = true,
            false => self.atoms.entry(literal.id().to_owned()).or_default().1 = true,
        }
    }

    pub fn setify_outer(&mut self) {
        self.sets.sort();
        self.sets.dedup();
    }

    pub fn sort_outer_and_inner(&mut self) {
        for set in &mut self.sets {
            set.set.sort_unstable();
        }

        self.sets.sort();
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn empty(mode: Mode) -> Self {
        FormulaSet {
            sets: vec![],
            atoms: HashMap::default(),
            mode,
        }
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn sets(&self) -> &Vec<LiteralSet<A>> {
        &self.sets
    }

    pub fn mode(&self) -> Mode {
        self.mode
    }

    pub fn filter_contradictions(&mut self) {
        match self.mode {
            Mode::CNF => todo!(),
            Mode::DNF => self.dnf_filter_contradictions(),
        }
    }

    pub fn is_bot(&self) -> bool {
        match self.mode {
            Mode::CNF => self.is_cnf_bot(),
            Mode::DNF => self.is_dnf_bot(),
        }
    }

    pub fn is_top(&self) -> bool {
        match self.mode {
            Mode::CNF => self.is_cnf_top(),
            Mode::DNF => self.is_dnf_top(),
        }
    }

    pub fn subsume(&mut self) {
        match self.mode {
            Mode::CNF => todo!(),
            Mode::DNF => self.dnf_subsume(),
        }
    }

    pub fn as_formula(&self) -> Formula<A> {
        match self.mode {
            Mode::CNF => self.cnf_formula(),
            Mode::DNF => self.dnf_formula(),
        }
    }

    pub fn set_contains_complementary_literals(set: &LiteralSet<A>) -> bool {
        use std::cmp::Ordering::*;

        match set.get_negative_positive_split_index() {
            Some(index) => {
                let (p, n) = set.set.split_at(index);

                let mut p_index = 0;
                let mut n_index = 0;

                while p_index < p.len() && n_index < n.len() {
                    match p[p_index].atom().cmp(n[n_index].atom()) {
                        Less => p_index += 1,
                        Equal => return true,
                        Greater => n_index += 1,
                    }
                }
                false
            }
            None => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{
        first_order::FirstOrderFormula,
        formula_set::{FormulaSet, Mode},
    };

    #[test]
    fn complementary_literals() {
        let fm = FirstOrderFormula::from("(P(a) & ~P(a))");
        let fms = fm.to_set_direct(Mode::DNF);
        assert!(
            fms.sets()
                .iter()
                .any(FormulaSet::set_contains_complementary_literals)
        );

        let fm = FirstOrderFormula::from("(P(a) & ~P(b))");
        let fms = fm.to_set_direct(Mode::DNF);
        assert!(
            !fms.sets()
                .iter()
                .all(FormulaSet::set_contains_complementary_literals)
        );
    }

    #[test]
    fn negative_positive_split() {
        let fm = FirstOrderFormula::from("P(a) & ~P(a) & ~Q(c)");
        let fms = fm.to_set_direct(Mode::DNF);
        let split = fms
            .sets()
            .first()
            .unwrap()
            .get_negative_positive_split_index();
        assert_eq!(Some(2), split);

        let fm = FirstOrderFormula::from("~P(a) & ~P(a) & ~Q(c)");
        let fms = fm.to_set_direct(Mode::DNF);
        let split = fms
            .sets()
            .first()
            .unwrap()
            .get_negative_positive_split_index();
        assert_eq!(None, split);

        let fm = FirstOrderFormula::from("P(a) & P(b) & Q(c)");
        let fms = fm.to_set_direct(Mode::DNF);
        let split = fms
            .sets()
            .first()
            .unwrap()
            .get_negative_positive_split_index();
        assert_eq!(None, split);
    }
}
