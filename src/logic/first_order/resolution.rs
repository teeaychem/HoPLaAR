use crate::logic::{
    Literal,
    first_order::{
        Relation,
        unification::{UnificationFailure, Unifier},
    },
    formula_set::Clause,
};

impl Clause<Relation> {
    pub fn unifiable(&mut self, literals: Clause<Relation>) -> bool {
        let mut u = Unifier::default();

        for a in literals.negative_literals() {
            for b in literals.negative_literals() {
                if u.relations_unify(&a.atom, &b.atom).is_ok() {
                    return true;
                }
            }
        }

        for a in literals.positive_literals() {
            for b in literals.positive_literals() {
                if u.relations_unify(&a.atom, &b.atom).is_ok() {
                    return true;
                }
            }
        }

        false
    }

    pub fn resolve_on(
        &mut self,
        other: &mut Clause<Relation>,
        literal: Literal<Relation>,
        resolvents: &mut [Clause<Relation>],
    ) {
        let clause_a = {
            let mut clause = self.clone();
            clause.prefix_variables("x".to_owned());
            clause
        };

        let clause_b = {
            let mut clause = other.clone();
            clause.prefix_variables("y".to_owned());
            clause
        };

        let mut u = Unifier::default();

        let ps2: Vec<_> = clause_b
            .literals_of_polarity(!literal.value)
            .filter(|l| u.relations_unifiable(&literal.atom, &l.atom))
            .collect();

        if ps2.is_empty() {
            return;
        }

        let ps1: Vec<_> = clause_a
            .literals_of_polarity(literal.value)
            .filter(|l| l.atom != literal.atom && u.relations_unifiable(&literal.atom, &l.atom))
            .collect();

        todo!()
    }
}

impl Unifier {
    pub fn most_general_unifier(
        &mut self,
        literals: Clause<Relation>,
    ) -> Result<usize, UnificationFailure> {
        let mut unifications = 0;

        for a in literals.negative_literals() {
            for b in literals.negative_literals() {
                unifications += self.relations_unify(&a.atom, &b.atom)?;
            }
        }

        for a in literals.positive_literals() {
            for b in literals.positive_literals() {
                unifications += self.relations_unify(&a.atom, &b.atom)?;
            }
        }

        self.solve();

        Ok(unifications)
    }
}

#[cfg(test)]
pub mod tests {
    use crate::logic::{
        first_order::{FirstOrderFormula, unification::Unifier},
        formula_set::FormulaSet,
    };

    #[test]
    fn barber() {
        let barber = "¬∃b.(∀x.(shaves(b, x) ↔ ¬shaves(x, x)))";
        let bf = FirstOrderFormula::from(barber);
        println!("{bf}");
        let bf = bf.clone().negate();
        println!("{bf}");
        let bf = bf.skolemize();
        println!("{bf}");
        let mut bf_cnf = FormulaSet::CNF(bf);
        println!("{bf_cnf}");
        let mut u = Unifier::default();
        for set in bf_cnf.sets() {
            println!("{:?}", u.most_general_unifier(set.clone()));
        }

        for (i, set) in bf_cnf.sets_mut().iter_mut().enumerate() {
            set.prefix_variables(format!("{i}$"));
        }

        println!("{bf_cnf}");
    }
}
