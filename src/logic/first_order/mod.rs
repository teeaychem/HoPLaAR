mod terms;
pub use terms::{Term, TermId};

mod relations;
pub use relations::Relation;

use crate::logic::Formula;

pub type FirstOrderFormula = Formula<Relation>;
