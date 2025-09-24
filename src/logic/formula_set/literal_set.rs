use std::collections::HashSet;

use crate::logic::{
    Atomic, Formula, Literal, OpBinary,
    first_order::{Relation, Term, terms::Var},
};

#[derive(Clone, Debug)]
pub struct LiteralSet<A: Atomic> {
    n: Vec<Literal<A>>,
    p: Vec<Literal<A>>,
}

impl<A: Atomic> LiteralSet<A> {
    pub fn len(&self) -> usize {
        self.n.len() + self.p.len()
    }

    pub fn is_empty(&self) -> bool {
        self.n.is_empty() && self.p.is_empty()
    }

    pub fn negative_positive_split(&self) -> (&[Literal<A>], &[Literal<A>]) {
        (&self.n, &self.p)
    }

    pub fn negative_literals(&self) -> std::slice::Iter<'_, Literal<A>> {
        self.n.iter()
    }

    pub fn positive_literals(&self) -> std::slice::Iter<'_, Literal<A>> {
        self.p.iter()
    }

    pub fn literals(
        &self,
    ) -> std::iter::Chain<std::slice::Iter<'_, Literal<A>>, std::slice::Iter<'_, Literal<A>>> {
        self.negative_literals().chain(self.positive_literals())
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

    pub fn atom_value(&self, atom: &A) -> Option<bool> {
        let (n, p) = self.negative_positive_split();

        for e in n {
            if e.atom() == atom {
                return Some(false);
            }
        }

        for e in p {
            if e.atom() == atom {
                return Some(true);
            }
        }

        None
    }

    pub fn remove_atom(&mut self, atom: &A) -> Option<Literal<A>> {
        for index in 0..self.n.len() {
            if self.n[index].atom() == atom {
                return Some(self.n.swap_remove(index));
            }
        }

        for index in 0..self.p.len() {
            if self.p[index].atom() == atom {
                return Some(self.p.swap_remove(index));
            }
        }

        None
    }
}

pub enum LiteralQuery {
    Missing,
    Matching,
    Conflicting,
}

impl<A: Atomic> LiteralSet<A> {
    pub fn sort(&mut self) {
        self.n.sort_unstable();
        self.p.sort_unstable();
    }

    pub fn setify(&mut self) {
        self.sort();
        self.n.dedup();
        self.p.dedup();
    }

    pub fn extend<I: IntoIterator<Item = Literal<A>>>(&mut self, iter: I) {
        for literal in iter {
            match literal.value() {
                false => self.n.push(literal),
                true => self.p.push(literal),
            }
        }

        self.setify();
    }

    pub fn negative_literals_mut(&mut self) -> std::slice::IterMut<'_, Literal<A>> {
        self.n.iter_mut()
    }

    pub fn positive_literals_mut(&mut self) -> std::slice::IterMut<'_, Literal<A>> {
        self.p.iter_mut()
    }

    pub fn literals_mut(
        &mut self,
    ) -> std::iter::Chain<std::slice::IterMut<'_, Literal<A>>, std::slice::IterMut<'_, Literal<A>>>
    {
        self.n.iter_mut().chain(self.p.iter_mut())
    }

    pub fn one_literal(
        &mut self,
        literal: &Literal<A>,
        remove_complementary: bool,
    ) -> LiteralQuery {
        for index in 0..self.n.len() {
            if self.n[index].atom() == literal.atom() {
                if self.n[index].value() == literal.value() {
                    return LiteralQuery::Matching;
                } else {
                    if remove_complementary {
                        self.n.swap_remove(index);
                    }

                    return LiteralQuery::Conflicting;
                }
            }
        }

        for index in 0..self.p.len() {
            if self.p[index].atom() == literal.atom() {
                if self.p[index].value() == literal.value() {
                    return LiteralQuery::Matching;
                } else {
                    if remove_complementary {
                        self.p.swap_remove(index);
                    }

                    return LiteralQuery::Conflicting;
                }
            }
        }

        LiteralQuery::Missing
    }
}

impl<A: Atomic> LiteralSet<A> {
    pub fn into_literals(
        self,
    ) -> std::iter::Chain<std::vec::IntoIter<Literal<A>>, std::vec::IntoIter<Literal<A>>> {
        self.n.into_iter().chain(self.p)
    }
}

impl LiteralSet<Relation> {
    /// Extend `collection` with the variables of `self`.
    pub fn extend_collection_with_variables<C: Extend<Var>>(&self, collection: &mut C) {
        for literal in self.n.iter().chain(self.p.iter()) {
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
        self.extend_collection_with_variables(&mut fvs);
        fvs
    }
}

// Trait impls

// Construction

impl<A: Atomic> Default for LiteralSet<A> {
    fn default() -> Self {
        Self {
            n: Default::default(),
            p: Default::default(),
        }
    }
}

impl<A: Atomic, LiteralIter: Iterator<Item = Literal<A>>> From<LiteralIter> for LiteralSet<A> {
    fn from(value: LiteralIter) -> Self {
        let mut n = Vec::default();
        let mut p = Vec::default();

        for literal in value {
            match literal.value() {
                true => p.push(literal),
                false => n.push(literal),
            }
        }

        let mut ls = Self { n, p };
        ls.setify();
        ls
    }
}

impl<A: Atomic> From<Literal<A>> for LiteralSet<A> {
    fn from(value: Literal<A>) -> Self {
        match value.value() {
            true => Self {
                n: vec![],
                p: vec![value],
            },
            false => Self {
                n: vec![value],
                p: vec![],
            },
        }
    }
}

// Eq / Ord

impl<A: Atomic> std::cmp::PartialEq for LiteralSet<A> {
    fn eq(&self, other: &Self) -> bool {
        self.n == other.n && self.p == other.p
    }
}

impl<A: Atomic> std::cmp::Eq for LiteralSet<A> {}

impl<A: Atomic> std::cmp::Ord for LiteralSet<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;

        match self.len().cmp(&other.len()) {
            Less => Less,
            Greater => Greater,
            Equal => {
                let self_lits = self.n.iter().chain(self.p.iter());
                let other_lits = other.n.iter().chain(other.p.iter());

                for (s, o) in self_lits.zip(other_lits) {
                    match s.cmp(o) {
                        Less => return Less,
                        Greater => return Greater,
                        Equal => continue,
                    }
                }

                Equal
            }
        }
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
        let set = value.0;
        let op = value.1;

        let mut literals = set.into_literals();
        let mut formula = match literals.next() {
            Some(literal) => Formula::from(literal),
            None => return Formula::True,
        };

        while let Some(literal) = literals.next() {
            formula = Formula::Binary(op, formula, Formula::from(literal));
        }

        formula
    }
}

// Etc...

impl<A: Atomic> std::fmt::Display for LiteralSet<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner_limit = self.len().saturating_sub(1);
        write!(f, "{{")?;
        for (inner_idx, literal) in self.n.iter().chain(self.p.iter()).enumerate() {
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
