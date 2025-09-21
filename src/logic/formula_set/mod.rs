mod cnf;
mod dnf;
mod literal_set;
use std::collections::{HashMap, HashSet};

pub use literal_set::LiteralSet;

mod propositional;
mod transformations;

use crate::logic::{
    Atomic, Formula, Literal,
    first_order::{Relation, terms::Var},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    CNF,
    DNF,
}

type AtomCache = HashMap<String, (bool, bool)>;

// A formula, as a set of sets.
// Invariant: `formula` is sorted by `literal_set_cmp`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FormulaSet<A: Atomic> {
    sets: Vec<LiteralSet<A>>,
    atoms: AtomCache,
    mode: Mode,
}

impl<A: Atomic> std::fmt::Display for FormulaSet<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let outer_limit = self.sets.len().saturating_sub(1);

        write!(f, "{{")?;
        for (outer_idx, expr) in self.sets.iter().enumerate() {
            write!(f, "{expr}")?;

            if outer_idx < outer_limit {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl<A: Atomic> Formula<A> {
    pub fn to_set_direct(&self, mode: Mode) -> FormulaSet<A> {
        let mut fs = FormulaSet::empty(mode);

        let mut sets = match mode {
            Mode::CNF => self.to_cnf_set_local(),
            Mode::DNF => self.to_dnf_set_local(),
        };

        for literal in sets.iter().flat_map(|s| s.literals()) {
            fs.cache_literal(literal);
        }

        std::mem::swap(&mut fs.sets, &mut sets);

        fs.setify_outer();

        fs
    }
}

impl<A: Atomic> FormulaSet<A> {
    fn update_atom_cache(cache: &mut AtomCache, literal: &Literal<A>) {
        match literal.value() {
            true => cache.entry(literal.id().to_owned()).or_default().0 = true,
            false => cache.entry(literal.id().to_owned()).or_default().1 = true,
        }
    }

    pub fn cache_literal(&mut self, literal: &Literal<A>) {
        Self::update_atom_cache(&mut self.atoms, literal);
    }

    pub fn refresh_literal_cache(&mut self) {
        let mut atom_cache = std::mem::take(&mut self.atoms);
        atom_cache.clear();

        for set in &self.sets {
            for literal in set.literals() {
                Self::update_atom_cache(&mut atom_cache, literal);
            }
        }

        let _ = std::mem::replace(&mut self.atoms, atom_cache);
    }

    pub fn setify_outer(&mut self) {
        self.sets.sort_unstable();
        self.sets.dedup();
    }

    pub fn sort_outer_and_inner(&mut self) {
        for set in &mut self.sets {
            set.sort();
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

    pub fn set_at_index(&self, index: usize) -> &LiteralSet<A> {
        &self.sets[index]
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

impl FormulaSet<Relation> {
    pub fn extend_with_variables<C: Extend<Var>>(&self, collection: &mut C) {
        for set in &self.sets {
            set.extend_with_variables(collection);
        }
    }

    pub fn variable_set(&self) -> HashSet<Var> {
        let mut fvs = HashSet::default();
        self.extend_with_variables(&mut fvs);
        fvs
    }

    pub fn on_variables<F: Fn(&mut Var)>(&mut self, op: F) {
        for set in &mut self.sets {
            for literal in set.literals_mut() {
                for term in &mut literal.atom_mut().terms {
                    for var in term.vars_mut_depth() {
                        op(var)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::{
        first_order::{FirstOrderFormula, terms::Var},
        formula_set::Mode,
    };

    #[test]
    fn complementary_literals() {
        let fm = FirstOrderFormula::from("(P(a) & ~P(a))");
        let fms = fm.to_set_direct(Mode::DNF);
        assert!(
            fms.sets()
                .iter()
                .any(|set| set.has_complementary_literals())
        );

        let fm = FirstOrderFormula::from("(P(a) & ~P(b))");
        let fms = fm.to_set_direct(Mode::DNF);
        assert!(
            !fms.sets()
                .iter()
                .all(|set| set.has_complementary_literals())
        );
    }

    #[test]
    fn free_variables() {
        let fm = FirstOrderFormula::from("(P(x) & ~P(y) | P(z))");
        let fms = fm.to_set_direct(Mode::DNF);
        let variables = fms.variable_set();

        let expected = HashSet::from(["x", "y", "z"].map(Var::from));

        assert_eq!(variables, expected)
    }
}
