use crate::logic::{
    Atomic, Formula, Literal, OpBinary, OpUnary,
    formula_set::{FormulaSet, Mode, literal_set_cmp, literal_set_to_formula},
};

impl<A: Atomic> Formula<A> {
    pub fn to_dnf_formula_set(&self) -> FormulaSet<A> {
        let mut formula = self.to_dnf_set();
        formula.sort_by(|a, b| literal_set_cmp(a, b));
        formula.dedup();

        FormulaSet {
            sets: formula,
            mode: Mode::DNF,
        }
    }

    fn to_dnf_set(&self) -> Vec<Vec<Literal<A>>> {
        match self {
            Formula::True => vec![vec![]],
            Formula::False => vec![],

            Formula::Atom(atom) => vec![vec![Literal::from(atom.clone(), true)]],

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => {
                    if let Formula::Atom(atom) = expr.as_ref() {
                        vec![vec![Literal::from(atom.clone(), false)]]
                    } else {
                        panic!()
                    }
                }
            },

            Formula::Binary { op, lhs, rhs } => {
                let lhs = lhs.to_dnf_formula_set();
                let rhs = rhs.to_dnf_formula_set();

                match op {
                    OpBinary::And => {
                        let mut fm = Vec::with_capacity(lhs.sets.len() * rhs.sets.len());
                        for l_set in &lhs.sets {
                            for r_set in &rhs.sets {
                                let mut product: Vec<Literal<A>> =
                                    l_set.iter().chain(r_set).cloned().collect();

                                // 'Setify'
                                // As the product is partially sorted, stable sort is preferred.
                                product.sort();
                                product.dedup();

                                fm.push(product);
                            }
                        }
                        fm
                    }

                    OpBinary::Or => lhs.sets.into_iter().chain(rhs.sets).collect(),

                    OpBinary::Imp | OpBinary::Iff => panic!(),
                }
            }

            Formula::Quantified { .. } => todo!(),
        }
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn dnf_filter_contradictions(&mut self) {
        let mut limit = self.sets.len();
        let mut set_idx = 0;

        'set_loop: while set_idx < limit {
            if self.sets[set_idx].len() > 1 {
                for idx in 1..self.sets[set_idx].len() {
                    if self.sets[set_idx][idx - 1].atom() == self.sets[set_idx][idx].atom() {
                        self.sets.swap_remove(set_idx);
                        limit -= 1;
                        continue 'set_loop;
                    }
                }
            }
            set_idx += 1;
        }

        self.sets.sort_by(|a, b| literal_set_cmp(a, b));
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

        'set_loop: while set_idx < limit {
            let base_set = &self.sets[set_idx];
            for (literal_idx, literal) in base_set.iter().enumerate() {
                if literal != &self.sets[set_idx + 1][literal_idx] {
                    set_idx += 1;
                    continue 'set_loop;
                }
            }

            self.sets.remove(set_idx);
            limit -= 1;
        }
    }

    pub fn dnf_formula(&self) -> Formula<A> {
        match self.sets.as_slice() {
            [] => Formula::False,
            [conjunction] => literal_set_to_formula(OpBinary::And, conjunction),
            [first, remaining @ ..] => {
                let mut formula = literal_set_to_formula(OpBinary::And, first);
                for other in remaining {
                    formula = Formula::Or(formula, literal_set_to_formula(OpBinary::And, other))
                }
                formula
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, parse_propositional};

    #[test]
    fn dnf_set() {
        let expr = parse_propositional("(p | q & r) & (~p | ~r)");
        let mut dnf = expr.to_dnf_formula_set();
        assert_eq!(dnf.sets().len(), 4);

        dnf.dnf_filter_contradictions();
        assert_eq!(dnf.sets().len(), 2);

        let expr = parse_propositional("(p | p & r)");
        let dnf = expr.to_dnf_formula_set();
        assert_eq!(dnf.sets().len(), 2);

        let expr = parse_propositional("false");
        let dnf = expr.to_dnf_formula_set();
        assert!(dnf.is_bot());

        let expr = parse_propositional("p & ~p");
        let mut dnf = expr.to_dnf_formula_set();
        dnf.filter_contradictions();
        assert!(dnf.is_bot());

        let expr = parse_propositional("true");
        let dnf = expr.to_dnf_formula_set();
        assert!(dnf.is_top());
    }

    #[test]
    fn dnf_subsumption() {
        let expr = parse_propositional("(p | p & r) & true");
        let mut dnf = expr.to_dnf_formula_set();
        dnf.subsume();
        assert!(dnf.sets().first().is_some_and(|set| set.len() == 2));
    }

    #[test]
    fn dnf_formula() {
        let expr = parse_propositional("(p | q & r) & (~p | ~r)");
        let dnf = expr.to_dnf_formula_set();
        let fm = dnf.as_formula();
        assert!(Formula::Iff(expr, fm).is_tautology());
    }
}
