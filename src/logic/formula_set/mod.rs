mod cnf;
mod dnf;
mod propositional;
mod transformations;

use std::collections::HashMap;

use crate::logic::{Atomic, Formula, Literal, OpBinary};

// Invariant: Literals are sorted by `literal_cmp`.
type LiteralSet<A> = Vec<Literal<A>>;

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

fn literal_set_cmp<A: Atomic>(a: &LiteralSet<A>, b: &LiteralSet<A>) -> std::cmp::Ordering {
    use std::cmp::Ordering::*;

    if a.is_empty() {
        return Less;
    }
    if b.is_empty() {
        return Greater;
    }

    let limit = std::cmp::min(a.len(), b.len());
    for idx in 0..limit {
        match a[idx].cmp(&b[idx]) {
            Less => return Less,
            Greater => return Greater,
            Equal => continue,
        }
    }

    a.len().cmp(&b.len())
}

fn literal_set_to_formula<A: Atomic>(op: OpBinary, ls: &LiteralSet<A>) -> Formula<A> {
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

impl<A: Atomic> std::fmt::Display for FormulaSet<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let outer_limit = self.sets.len().saturating_sub(1);

        let _ = write!(f, "{{");
        for (outer_idx, expr) in self.sets.iter().enumerate() {
            let inner_limit = expr.len().saturating_sub(1);

            let _ = write!(f, "{{");
            for (inner_idx, literal) in expr.iter().enumerate() {
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
        sets.sort_by(|a, b| literal_set_cmp(a, b));
        sets.dedup();

        for literal in sets.iter().flatten() {
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
        self.sets.sort_by(|a, b| literal_set_cmp(a, b));
        self.sets.dedup();
    }

    pub fn sort_outer_and_inner(&mut self) {
        for set in &mut self.sets {
            set.sort_unstable();
        }

        self.sets.sort_by(|a, b| literal_set_cmp(a, b));
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

    /// Returns the index which separates negative literals from positive literals, if such an index exists.
    /// If all literals share the same value, None is returned.
    /// Note, as a consequence of the above, the returned index is never 0.
    /// (For, otherwise all literals must be positive.)
    pub fn get_negative_positive_split(set: &[Literal<A>]) -> Option<usize> {
        for (index, literal) in set.iter().enumerate() {
            if literal.value() {
                match index {
                    0 => return None,
                    _ => return Some(index),
                }
            }
        }
        None
    }

    pub fn set_contains_complementary_literals(set: &LiteralSet<A>) -> bool {
        use std::cmp::Ordering::*;

        match Self::get_negative_positive_split(set) {
            Some(index) => {
                let (p, n) = set.split_at(index);

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

pub fn setify<A: Atomic>(set: &mut Vec<Literal<A>>) {
    set.sort_unstable();
    set.dedup();
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
        let split = FormulaSet::get_negative_positive_split(fms.sets().first().unwrap());
        assert_eq!(Some(2), split);

        let fm = FirstOrderFormula::from("~P(a) & ~P(a) & ~Q(c)");
        let fms = fm.to_set_direct(Mode::DNF);
        let split = FormulaSet::get_negative_positive_split(fms.sets().first().unwrap());
        assert_eq!(None, split);

        let fm = FirstOrderFormula::from("P(a) & P(b) & Q(c)");
        let fms = fm.to_set_direct(Mode::DNF);
        let split = FormulaSet::get_negative_positive_split(fms.sets().first().unwrap());
        assert_eq!(None, split);
    }
}
