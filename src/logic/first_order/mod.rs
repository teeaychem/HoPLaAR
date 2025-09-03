mod domains;
pub use domains::Element;

mod semantics;
pub use semantics::{
    InterpretationF, InterpretationR, Model, Valuation, eval_first_order, eval_relation, eval_term,
};

pub mod terms;

pub use terms::{Term, TermId};

mod relations;
pub use relations::Relation;

pub use crate::logic::parse::parse_first_order as parse;

use crate::logic::Formula;

pub type FirstOrderFormula = Formula<Relation>;

pub type InterpretationBool = Model<bool>;

impl FirstOrderFormula {
    #[allow(non_snake_case)]
    pub fn eval<D: Element>(&self, M: &Model<D>, v: &mut Valuation<D>) -> bool {
        eval_first_order(self, M, v)
    }
}
