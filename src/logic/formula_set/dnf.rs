use crate::logic::{
    Atomic, Formula, Literal, OpBinary, OpUnary,
    formula_set::{FormulaSet, LiteralSet, Mode},
};

impl<A: Atomic> Formula<A> {
    pub(super) fn into_dnf_set_local(self) -> FormulaSet<A> {
        let mut fs = FormulaSet::empty(Mode::DNF);

        match self {
            Formula::True => fs.add_literal_set(LiteralSet::default()),
            Formula::False => {}

            Formula::Atom(atom) => fs.add_literal_set(LiteralSet::from(Literal::from(atom, true))),

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => {
                    if let Formula::Atom(atom) = *expr {
                        fs.add_literal_set(LiteralSet::from(Literal::from(atom, false)));
                    } else {
                        panic!()
                    }
                }
            },

            Formula::Binary { op, lhs, rhs } => {
                let lhs = lhs.to_set_direct(Mode::DNF);
                let rhs = rhs.to_set_direct(Mode::DNF);

                match op {
                    OpBinary::And => {
                        for l_set in &lhs.sets {
                            for r_set in &rhs.sets {
                                let product = LiteralSet::from(
                                    l_set.literals().chain(r_set.literals()).cloned(),
                                );
                                fs.add_literal_set(product);
                            }
                        }
                    }

                    OpBinary::Or => {
                        for set in lhs.sets.into_iter().chain(rhs.sets) {
                            fs.add_literal_set(set);
                        }
                    }

                    OpBinary::Imp | OpBinary::Iff => panic!(),
                }
            }

            Formula::Quantified { .. } => todo!(),
        };

        fs
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn dnf_filter_contradictions(&mut self) {
        let mut limit = self.sets.len();
        let mut set_idx = 0;

        while set_idx < limit {
            if self.sets[set_idx].len() > 1 && self.sets[set_idx].has_complementary_literals() {
                self.sets.swap_remove(set_idx);
                limit -= 1;
            } else {
                set_idx += 1;
            }
        }

        self.sets.sort();
    }

    pub fn is_dnf_bot(&self) -> bool {
        self.sets.is_empty()
    }

    pub fn is_dnf_top(&self) -> bool {
        !self.sets.is_empty() && self.sets.iter().all(|set| set.is_empty())
    }

    // DNF subsumption
    //
    // Formula sets are ordered by inclusion.
    // So from left to right, if all elements of A are in B, A ⊆ B.
    // And, then, A should be removed while B is preserved, as A and B are conjuncts.
    pub fn dnf_subsume(&mut self) {
        let mut limit = self.sets.len() - 1;
        let mut set_idx = 0;

        while set_idx < limit {
            match self.sets[set_idx].is_subset_of(&self.sets[set_idx + 1]) {
                true => {
                    self.sets.remove(set_idx);
                    limit -= 1
                }
                false => set_idx += 1,
            }
        }
    }

    pub fn dnf_formula(&self) -> Formula<A> {
        match self.sets.as_slice() {
            [] => Formula::False,
            [conjunction] => Formula::from((conjunction.clone(), OpBinary::And)),
            [first, remaining @ ..] => {
                let mut formula = Formula::from((first.clone(), OpBinary::And));
                for other in remaining {
                    formula = Formula::Or(formula, Formula::from((other.clone(), OpBinary::And)))
                }
                formula
            }
        }
    }

    pub fn dnf_conjoin(self, other: FormulaSet<A>) -> FormulaSet<A> {
        let mut conjunction = FormulaSet::empty(Mode::DNF);

        for self_literal_set in self.sets {
            for other_literal_set in &other.sets {
                let mut merge = LiteralSet {
                    n: self_literal_set
                        .negative_literals()
                        .chain(other_literal_set.negative_literals())
                        .cloned()
                        .collect(),
                    p: self_literal_set
                        .positive_literals()
                        .chain(other_literal_set.positive_literals())
                        .cloned()
                        .collect(),
                    index: 0,
                };
                merge.setify();

                conjunction.add_literal_set(merge);
            }
        }

        conjunction.refresh_literal_cache();

        conjunction.setify_outer();

        conjunction
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{
        Formula, first_order::FirstOrderFormula, formula_set::Mode, parse_propositional,
    };

    #[test]
    fn dnf_set_bot() {
        let expr = parse_propositional("false");
        let dnf = expr.to_set_direct(Mode::DNF);
        assert!(dnf.is_bot());

        let expr = parse_propositional("p & ~p");
        let mut dnf = expr.to_set_direct(Mode::DNF);
        dnf.filter_contradictions();
        assert!(dnf.is_bot());
    }

    #[test]
    fn dnf_set_top() {
        let expr = parse_propositional("true");
        let dnf = expr.to_set_direct(Mode::DNF);
        assert!(dnf.is_top());
    }

    #[test]
    fn dnf_set() {
        let expr = parse_propositional("(p | q & r) & (~p | ~r)");
        let mut dnf = expr.to_set_direct(Mode::DNF);
        assert_eq!(dnf.sets().len(), 4);

        dnf.dnf_filter_contradictions();
        assert_eq!(dnf.sets().len(), 2);

        let expr = parse_propositional("(p | p & r)");
        let dnf = expr.to_set_direct(Mode::DNF);
        assert_eq!(dnf.sets().len(), 2);
    }

    #[test]
    fn dnf_subsumption() {
        let expr = parse_propositional("(p | p & r) & true");
        let mut dnf = expr.to_set_direct(Mode::DNF);
        dnf.subsume();
        assert!(dnf.sets().first().is_some_and(|set| set.len() == 2));
    }

    #[test]
    fn dnf_formula() {
        let expr = parse_propositional("(p | q & r) & (~p | ~r)");
        let dnf = expr.clone().to_set_direct(Mode::DNF);

        let fm = dnf.as_formula();

        assert!(Formula::Iff(expr, fm).is_tautology());
    }

    #[test]
    fn dnf_conjoin() {
        let fax = FirstOrderFormula::from("(P(x) & P(y)) | (~P(x) & ~P(y))");
        let fmas = fax.to_set_direct(Mode::DNF);

        let fmb = FirstOrderFormula::from("P(y) | ~P(y)");
        let fmbs = fmb.to_set_direct(Mode::DNF);

        let fme = FirstOrderFormula::from(
            "(P(x) & P(y))
           | (~P(x) & ~P(y) & P(y))
           | (P(x) & P(y) & ~P(y))
           | (~P(x) & ~P(y))",
        );
        let fmes = fme.to_set_direct(Mode::DNF);

        let fmcs = fmas.dnf_conjoin(fmbs);

        assert_eq!(fmcs, fmes);
    }
}
