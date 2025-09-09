use crate::logic::{
    formula_set::{FormulaSet, Mode, literal_set_cmp},
    propositional::{Prop, PropFormula},
};

impl PropFormula {
    pub fn to_cnf_formula_set_tseytin(&self) -> FormulaSet<Prop> {
        let (_, cnf) = self.clone().cnf();
        let mut formula = cnf.to_cnf_set();
        formula.sort_by(literal_set_cmp);
        formula.dedup();

        FormulaSet {
            sets: formula,
            mode: Mode::CNF,
        }
    }
}
