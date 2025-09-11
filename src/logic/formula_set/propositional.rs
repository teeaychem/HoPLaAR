use crate::logic::{
    formula_set::FormulaSet,
    propositional::{Prop, PropFormula},
};

impl PropFormula {
    pub fn to_cnf_formula_set_tseytin(&self) -> FormulaSet<Prop> {
        let (_, cnf) = self.clone().cnf();
        cnf.to_cnf_set_direct()
    }
}
