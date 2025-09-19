use crate::logic::{Atomic, Literal};

#[derive(Clone, Debug)]
pub struct LiteralSet<A: Atomic> {
    pub set: Vec<Literal<A>>,
}

impl<A: Atomic> Default for LiteralSet<A> {
    fn default() -> Self {
        Self {
            set: Default::default(),
        }
    }
}

impl<A: Atomic> LiteralSet<A> {
    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }
}

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

impl<A: Atomic> LiteralSet<A> {
    pub fn as_slice(&self) -> &[Literal<A>] {
        self.set.as_slice()
    }

    /// Returns the index which separates negative literals from positive literals, if such an index exists.
    /// If all literals share the same value, None is returned.
    /// Note, as a consequence of the above, the returned index is never 0.
    /// (For, otherwise all literals must be positive.)
    pub fn get_negative_positive_split_index(&self) -> Option<usize> {
        for (index, literal) in self.set.iter().enumerate() {
            if literal.value() {
                match index {
                    0 => return None,
                    _ => return Some(index),
                }
            }
        }
        None
    }

    pub fn get_negative_positive_splits(&self) -> (&[Literal<A>], &[Literal<A>]) {
        match self.get_negative_positive_split_index() {
            Some(index) => self.set.split_at(index),
            None => (&[], &[]),
        }
    }
}

impl<A: Atomic> LiteralSet<A> {
    pub fn setify(&mut self) {
        self.set.sort_unstable();
        self.set.dedup();
    }
}

impl<A: Atomic, I: Iterator<Item = Literal<A>>> From<I> for LiteralSet<A> {
    fn from(value: I) -> Self {
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
