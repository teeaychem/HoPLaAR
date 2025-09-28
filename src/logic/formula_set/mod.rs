mod cnf;
mod dnf;
mod literal_set;
use std::collections::{HashMap, HashSet};

pub use literal_set::LiteralSet;

mod propositional;
mod transformations;

use crate::logic::{
    Atomic, Formula,
    first_order::{Relation, terms::Var},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    CNF,
    DNF,
}

type AtomCache<A> = HashMap<A, (bool, bool)>;
type OccurrenceMap<A> = HashMap<A, (usize, usize)>;
type VariableSet = HashSet<Var>;

// A formula, as a set of sets.
// Invariant: `formula` is sorted by `literal_set_cmp`.
#[derive(Clone, Debug)]
pub struct FormulaSet<A: Atomic> {
    sets: Vec<LiteralSet<A>>,
    mode: Mode,
    index: usize,
}

impl<A: Atomic> std::fmt::Display for FormulaSet<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (outer_idx, expr) in self.sets.iter().enumerate() {
            write!(f, "{expr}")?;

            if outer_idx + 1 < self.sets.len() {
                writeln!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl<A: Atomic> Formula<A> {
    pub fn to_set_direct(self, mode: Mode) -> FormulaSet<A> {
        let mut fs = match mode {
            Mode::CNF => self.into_cnf_set_local(),
            Mode::DNF => self.into_dnf_set_local(),
        };

        fs.setify_outer();

        fs
    }
}

impl<A: Atomic> FormulaSet<A> {
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

    pub fn add_set(&mut self, mut literal_set: LiteralSet<A>) {
        self.index += 1;
        literal_set.index = self.index;
        self.sets.push(literal_set);
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn empty(mode: Mode) -> Self {
        FormulaSet {
            sets: vec![],
            mode,
            index: 0,
        }
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn len(&self) -> usize {
        self.sets.len()
    }

    pub fn is_empty(&self) -> bool {
        self.sets.is_empty()
    }

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

    pub fn occurrence_map(&self) -> OccurrenceMap<A> {
        let mut map = OccurrenceMap::<A>::default();

        for n in self.sets.iter().flat_map(|s| s.negative_literals()) {
            map.entry(n.atom().clone()).or_default().0 += 1;
        }

        for p in self.sets.iter().flat_map(|s| s.positive_literals()) {
            map.entry(p.atom().clone()).or_default().1 += 1;
        }

        map
    }
}

impl FormulaSet<Relation> {
    pub fn extend_with_variables<C: Extend<Var>>(&self, collection: &mut C) {
        for set in &self.sets {
            set.extend_collection_with_variables(collection);
        }
    }

    pub fn variable_set(&self) -> VariableSet {
        let mut fvs = VariableSet::default();
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

impl<A: Atomic> std::cmp::PartialEq for FormulaSet<A> {
    fn eq(&self, other: &Self) -> bool {
        self.sets == other.sets && self.mode == other.mode
    }
}

impl<A: Atomic> std::cmp::Eq for FormulaSet<A> {}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::{
        Literal,
        first_order::{FirstOrderFormula, terms::Var},
        formula_set::{FormulaSet, LiteralSet, Mode},
        propositional::Prop,
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

    #[test]
    fn order() {
        let mut fm = FormulaSet::empty(Mode::CNF);

        let ls = LiteralSet::from(
            ["a", "b"]
                .into_iter()
                .map(|a| Literal::<Prop>::from(Prop::from(a), true)),
        );
        fm.add_set(ls);

        let ls = LiteralSet::from(
            ["c", "d"]
                .into_iter()
                .map(|a| Literal::<Prop>::from(Prop::from(a), true)),
        );
        fm.add_set(ls);

        let ls = LiteralSet::from(
            ["e"]
                .into_iter()
                .map(|a| Literal::<Prop>::from(Prop::from(a), true)),
        );
        fm.add_set(ls);

        let e = Literal::<Prop>::from(Prop::from("e"), true);

        assert!(
            !fm.sets
                .iter()
                .filter(|s| s.index == 1)
                .any(|l| l.literals().any(|l| l == &e))
        );

        assert!(
            fm.sets
                .iter()
                .filter(|s| s.index > 1)
                .any(|l| l.literals().any(|l| l == &e))
        );
    }
}
