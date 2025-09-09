use crate::logic::{
    Atomic, Formula, Literal, OpBinary, OpUnary,
    formula_set::{FormulaSet, Mode, literal_set_cmp, literal_set_to_formula},
};

impl<A: Atomic> FormulaSet<A> {
    pub fn is_cnf_bot(&self) -> bool {
        self.sets.first().is_some_and(|set| set.is_empty())
    }

    pub fn is_cnf_top(&self) -> bool {
        self.sets.is_empty()
    }

    // CNF subsumption
    //
    // Formula sets are ordered by inclusion.
    // So from left to right, if all elements of A are in B, A ⊆ B.
    // And, then, A should be removed while B is preserved, as A and B are disjuncts.
    pub fn cnf_subsume(&mut self) {
        let mut limit = self.sets.len();
        let mut set_idx = 1;

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

    pub fn cnf_formula(&self) -> Formula<A> {
        match self.sets.as_slice() {
            [] => Formula::False,
            [conjunction] => literal_set_to_formula(OpBinary::Or, conjunction),
            [first, remaining @ ..] => {
                let mut formula = literal_set_to_formula(OpBinary::Or, first);
                for other in remaining {
                    formula = Formula::And(formula, literal_set_to_formula(OpBinary::Or, other))
                }
                formula
            }
        }
    }
}

impl<A: Atomic> Formula<A> {
    pub fn to_cnf_set_direct(&self) -> FormulaSet<A> {
        let mut formula = self.to_cnf_set_local();
        formula.sort_by(|a, b| literal_set_cmp(a, b));
        formula.dedup();

        FormulaSet {
            sets: formula,
            mode: Mode::CNF,
        }
    }

    fn to_cnf_set_local(&self) -> Vec<Vec<Literal<A>>> {
        match self {
            Formula::True => vec![],
            Formula::False => vec![vec![]],

            Formula::Atom(atom) => vec![vec![Literal::from(atom.clone(), true)]],

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => {
                    if let Formula::Atom(atom) = expr.as_ref() {
                        vec![vec![Literal::from(atom.clone(), false)]]
                    } else {
                        todo!()
                    }
                }
            },

            Formula::Binary { op, lhs, rhs } => {
                let lhs = lhs.to_cnf_set_direct();
                let rhs = rhs.to_cnf_set_direct();

                match op {
                    OpBinary::Or => {
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

                    OpBinary::And => lhs.sets.into_iter().chain(rhs.sets).collect(),

                    OpBinary::Imp => todo!(),

                    OpBinary::Iff => todo!(),
                }
            }

            Formula::Quantified { .. } => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::parse_propositional;

    #[test]
    fn cnf_true_false() {
        let t = parse_propositional("true");
        assert!(t.to_cnf_set_direct().is_cnf_top());

        let f = parse_propositional("false");
        assert!(f.to_cnf_set_direct().is_cnf_bot());
}

    #[test]
    fn cnf_set() {
        let p_q = parse_propositional("p | r");
        let p_q_set = p_q.to_cnf_set_direct();

        let expr = parse_propositional("~p => r");
        let mut cnf = expr.to_cnf_formula_set_tseytin();
        cnf.one_literal_rule();
        assert_eq!(cnf, p_q_set)
    }
}
