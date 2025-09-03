mod domains;
pub use domains::Element;

mod semantics;
pub use semantics::{Model, Valuation, eval_first_order, eval_relation, eval_term};

pub mod terms;

pub use terms::{Term, TermId};

mod relations;
pub use relations::Relation;

pub use crate::logic::parse::parse_first_order as parse;

use crate::logic::Formula;

pub type FirstOrderFormula = Formula<Relation>;

impl FirstOrderFormula {
    #[allow(non_snake_case)]
    pub fn eval<E: Element, M: Model<E>>(&self, M: &M, v: &mut Valuation<E>) -> bool {
        eval_first_order(self, M, v)
    }
}
