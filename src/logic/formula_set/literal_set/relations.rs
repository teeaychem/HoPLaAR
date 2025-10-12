use crate::logic::{
    Literal,
    first_order::{Relation, Term, syntax::Substitution},
    formula_set::{FormulaSet, LiteralSet},
};

impl LiteralSet<Relation> {
    pub fn apply_subsitution(&mut self, substitution: &mut Substitution) {
        for relation in &mut self.n {
            relation.apply_substition(substitution);
        }

        for relation in &mut self.p {
            relation.apply_substition(substitution);
        }
    }

    pub fn prefix_variables(&mut self, prefix: String) {
        let pfx = move |t: Term| -> Term {
            match t {
                Term::F(_) => t,
                Term::V(mut v) => {
                    v.id = format!("{}{}", prefix, v.id);
                    Term::V(v)
                }
            }
        };
        let mut pfx_substituion = Substitution::from_function(Box::new(pfx));
        self.apply_subsitution(&mut pfx_substituion);
    }
}

impl FormulaSet<Relation> {
    pub fn apply_subsitution(&mut self, substitution: &mut Substitution) {
        for set in &mut self.sets {
            set.apply_subsitution(substitution);
        }
    }
}
