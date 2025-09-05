mod etc;

pub mod first_order;

pub mod formula_set;
pub mod iterators;

mod language;
use std::{cmp, fmt::Display, hash};

pub use language::{
    formulas::Formula,
    literals::Literal,
    logical_constants::{OpBinary, OpUnary, Quantifier},
};

mod parse;
pub use parse::parse_propositional;

pub mod propositional;

mod syntax;

pub trait Atomic: std::fmt::Debug + Display + Clone + hash::Hash + Eq + cmp::Ord {
    type Constant: Clone + Eq + cmp::Ord + Display + hash::Hash;
    type Function: Clone + Eq + cmp::Ord + Display + hash::Hash;
    type Variable: Clone + Eq + cmp::Ord + Display + hash::Hash;

    type Part: Clone + Eq + cmp::Ord + Display;

    // A string identifier which uniquely identifier the atom.
    fn id(&self) -> &str;

    fn parts(&self) -> impl Iterator<Item = &Self::Part>;

    fn constants(&self) -> impl Iterator<Item = &Self::Constant>;

    fn functions(&self) -> impl Iterator<Item = &Self::Function>;

    fn variables(&self) -> impl Iterator<Item = &Self::Variable>;
}
