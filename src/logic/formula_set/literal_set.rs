use std::collections::HashSet;

use crate::logic::{
    Atomic, Formula, Literal, OpBinary,
    first_order::{Relation, Term, terms::Var},
};

#[derive(Clone, Debug)]
pub struct LiteralSet<A: Atomic> {
    set: Vec<Literal<A>>,
}

impl<A: Atomic> LiteralSet<A> {
    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    pub fn negative_positive_split(&self) -> (&[Literal<A>], &[Literal<A>]) {
        for (index, literal) in self.set.iter().enumerate() {
            if literal.value() {
                match index {
                    0 => return (&[], &self.set),
                    _ => return self.set.split_at(index),
                }
            }
        }

        (&self.set, &[])
    }

    pub fn atom_at(&self, index: usize) -> &A {
        self.set[index].atom()
    }

    pub fn value_at(&self, index: usize) -> bool {
        self.set[index].value()
    }

    pub fn literals(&self) -> std::slice::Iter<'_, Literal<A>> {
        self.set.iter()
    }

    pub fn is_subset_of(&self, other: &LiteralSet<A>) -> bool {
        let (s_n, s_p) = self.negative_positive_split();
        let (o_n, o_p) = other.negative_positive_split();

        if s_n.len() > o_n.len() || s_p.len() > o_p.len() {
            return false;
        }

        for (s, o) in s_n.iter().zip(o_n[..s_n.len()].iter()) {
            if o.atom() != s.atom() {
                return false;
            }
        }

        for (s, o) in s_p.iter().zip(o_p[..s_p.len()].iter()) {
            if o.atom() != s.atom() {
                return false;
            }
        }

        true
    }

    pub fn some_complementary_literal_index(&self) -> Option<(usize, usize)> {
        use std::cmp::Ordering;

        let (n, p) = self.negative_positive_split();

        if n.is_empty() || p.is_empty() {
            return None;
        }

        let mut p_index = 0;
        let mut n_index = 0;

        while p_index < p.len() && n_index < n.len() {
            match p[p_index].atom().cmp(n[n_index].atom()) {
                Ordering::Less => p_index += 1,
                Ordering::Equal => {
                    return Some((n_index, p_index));
                }
                Ordering::Greater => n_index += 1,
            }
        }

        None
    }

    pub fn has_complementary_literals(&self) -> bool {
        self.some_complementary_literal_index().is_some()
    }
}

impl<A: Atomic> LiteralSet<A> {
    pub fn remove(&mut self, index: usize) -> Literal<A> {
        self.set.swap_remove(index)
    }

    pub fn sort(&mut self) {
        self.set.sort_unstable();
    }

    pub fn setify(&mut self) {
        self.sort();
        self.set.dedup();
    }

    pub fn extend<I: IntoIterator<Item = Literal<A>>>(&mut self, iter: I) {
        self.set.extend(iter);
        self.setify();
    }

    pub fn literals_mut(&mut self) -> std::slice::IterMut<'_, Literal<A>> {
        self.set.iter_mut()
    }
}

impl<A: Atomic> LiteralSet<A> {
    pub fn into_literals(self) -> std::vec::IntoIter<Literal<A>> {
        self.set.into_iter()
    }
}

impl LiteralSet<Relation> {
    /// Extend `collection` with the variables of `self`.
    pub fn extend_with_variables<C: Extend<Var>>(&self, collection: &mut C) {
        for literal in &self.set {
            for term in &literal.atom().terms {
                match term {
                    Term::F(fun) => {
                        for arg in &fun.args {
                            arg.extend_with_variables(collection);
                        }
                    }
                    Term::V(var) => {
                        collection.extend(std::iter::once(var.clone()));
                    }
                }
            }
        }
    }

    /// The variables of `self`, collected in a hash set.
    pub fn variables(&self) -> HashSet<Var> {
        let mut fvs = HashSet::default();
        self.extend_with_variables(&mut fvs);
        fvs
    }
}

// Trait impls

// Construction

impl<A: Atomic> Default for LiteralSet<A> {
    fn default() -> Self {
        Self {
            set: Default::default(),
        }
    }
}

impl<A: Atomic, LiteralIter: Iterator<Item = Literal<A>>> From<LiteralIter> for LiteralSet<A> {
    fn from(value: LiteralIter) -> Self {
        let set: Vec<Literal<A>> = value.collect();
        let mut ls = Self { set };
        ls.setify();
        ls
    }
}

impl<A: Atomic> From<Literal<A>> for LiteralSet<A> {
    fn from(value: Literal<A>) -> Self {
        Self { set: vec![value] }
    }
}

// Eq / Ord

impl<A: Atomic> std::cmp::PartialEq for LiteralSet<A> {
    fn eq(&self, other: &Self) -> bool {
        self.set == other.set
    }
}

impl<A: Atomic> std::cmp::Eq for LiteralSet<A> {}

impl<A: Atomic> std::cmp::Ord for LiteralSet<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;

        if self.is_empty() {
            return Less;
        }
        if other.is_empty() {
            return Greater;
        }

        let limit = std::cmp::min(self.len(), other.len());
        for idx in 0..limit {
            match self.set[idx].cmp(&other.set[idx]) {
                Less => return Less,
                Greater => return Greater,
                Equal => continue,
            }
        }

        self.len().cmp(&other.len())
    }
}

impl<A: Atomic> std::cmp::PartialOrd for LiteralSet<A> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// From

impl<A: Atomic> From<(LiteralSet<A>, OpBinary)> for Formula<A> {
    fn from(value: (LiteralSet<A>, OpBinary)) -> Self {
        match value.0.set.as_slice() {
            [] => Formula::True,
            [literal] => Formula::from(literal.clone()),
            [first, remaining @ ..] => {
                let mut formula = Formula::from(first.clone());
                for other in remaining {
                    formula = Formula::Binary(value.1, formula, Formula::from(other.clone()));
                }
                formula
            }
        }
    }
}

// Etc...

impl<A: Atomic> std::fmt::Display for LiteralSet<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner_limit = self.set.len().saturating_sub(1);
        write!(f, "{{")?;
        for (inner_idx, literal) in self.set.iter().enumerate() {
            write!(f, "{literal}")?;
            if inner_idx < inner_limit {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{first_order::FirstOrderFormula, formula_set::Mode};

    #[test]
    fn subset() {
        let a = FirstOrderFormula::from("P(a)");
        let a_set = &a.to_set_direct(Mode::DNF).sets[0];

        let ab = FirstOrderFormula::from("(P(a) & P(b))");
        let ab_set = &ab.to_set_direct(Mode::DNF).sets[0];

        assert!(!ab_set.is_subset_of(a_set));
        assert!(a_set.is_subset_of(ab_set));

        let anb = FirstOrderFormula::from("(P(a) & ~P(b))");
        let anb_set = &anb.to_set_direct(Mode::DNF).sets[0];

        assert!(!anb_set.is_subset_of(a_set));
        assert!(a_set.is_subset_of(anb_set));

        let nanb = FirstOrderFormula::from("(~P(a) & ~P(b))");
        let nanb_set = &nanb.to_set_direct(Mode::DNF).sets[0];

        assert!(!nanb_set.is_subset_of(a_set));
        assert!(!a_set.is_subset_of(nanb_set));
    }

    #[test]
    fn complementary() {
        let anb = FirstOrderFormula::from("(P(a) & ~P(b))");
        let anb_set = &anb.to_set_direct(Mode::DNF).sets[0];

        assert!(!anb_set.has_complementary_literals());

        let bnb = FirstOrderFormula::from("(P(b) & ~P(b))");
        let bnb_set = &bnb.to_set_direct(Mode::DNF).sets[0];

        assert!(bnb_set.has_complementary_literals());
    }

    #[test]
    fn negative_positive_split() {
        let fm = FirstOrderFormula::from("P(a) & ~P(a) & ~Q(c)");
        let fms = fm.to_set_direct(Mode::DNF);
        let (n, _) = fms.set_at_index(0).negative_positive_split();
        assert_eq!(2, n.len());

        let fm = FirstOrderFormula::from("~P(a) & ~P(a) & ~Q(c)");
        let fms = fm.to_set_direct(Mode::DNF);
        let (n, _) = fms.set_at_index(0).negative_positive_split();
        assert_eq!(2, n.len());

        let fm = FirstOrderFormula::from("P(a) & P(b) & Q(c)");
        let fms = fm.to_set_direct(Mode::DNF);
        let (n, _) = fms.set_at_index(0).negative_positive_split();
        assert_eq!(0, n.len());
    }
}
