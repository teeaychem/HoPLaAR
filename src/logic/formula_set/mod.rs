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
}

pub fn set_contains_complementary_literals<A: Atomic>(set: &LiteralSet<A>) -> bool {
    for idx in 1..set.len() {
        if set[idx - 1].id() == set[idx].id() {
            return true;
        }
    }
    false
}

pub fn setify<A: Atomic>(set: &mut LiteralSet<A>) {
    set.sort_unstable();
    set.dedup();
}
