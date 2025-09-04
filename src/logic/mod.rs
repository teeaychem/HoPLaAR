mod language;
pub use language::{
    formulas::Formula,
    literals::Literal,
    logical_constants::{OpBinary, OpUnary, Quantifier},
};

pub mod first_order;

pub mod formula_set;
pub mod iterators;

mod syntax;

mod parse;
pub use parse::{parse_first_order, parse_propositional};

pub mod propositional;

pub trait Atomic:
    std::fmt::Debug + std::fmt::Display + Clone + std::hash::Hash + Eq + std::cmp::Ord
{
    type Quantum: Clone + Eq + std::cmp::Ord + std::fmt::Display;

    // A string identifier which uniquely identifier the atom.
    fn id(&self) -> &str;

    fn variables(&self) -> impl Iterator<Item = Self::Quantum>;
}
