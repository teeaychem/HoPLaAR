use crate::logic::{
    Atomic, Formula, Literal, OpBinary, OpUnary,
    formula_set::{FormulaSet, LiteralSet, Mode},
};

impl<A: Atomic> FormulaSet<A> {
    pub fn is_cnf_bot(&self) -> bool {
        !self.sets.is_empty() && self.sets.iter().all(|set| set.is_empty())
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
        let mut limit = self.sets.len() - 1;
        let mut set_idx = 0;

        while set_idx < limit {
            match self.sets[set_idx].is_subset_of(&self.sets[set_idx + 1]) {
                true => set_idx += 1,
                false => {
                    self.sets.remove(set_idx);
                    limit -= 1
                }
            }
        }
    }

    pub fn cnf_formula(&self) -> Formula<A> {
        match self.sets.as_slice() {
            [] => Formula::False,
            [conjunction] => Formula::from((conjunction.clone(), OpBinary::Or)),
            [first, remaining @ ..] => {
                let mut formula = Formula::from((first.clone(), OpBinary::Or));
                for other in remaining {
                    formula = Formula::And(formula, Formula::from((other.clone(), OpBinary::Or)))
                }
                formula
            }
        }
    }
}

impl<A: Atomic> Formula<A> {
    pub(super) fn to_cnf_set_local(&self) -> Vec<LiteralSet<A>> {
        match self {
            Formula::True => vec![],
            Formula::False => vec![LiteralSet::default()],

            Formula::Atom(atom) => vec![LiteralSet::from(Literal::from(atom.clone(), true))],

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => {
                    if let Formula::Atom(atom) = expr.as_ref() {
                        vec![LiteralSet::from(Literal::from(atom.clone(), false))]
                    } else {
                        todo!()
                    }
                }
            },

            Formula::Binary { op, lhs, rhs } => {
                let lhs = lhs.to_set_direct(Mode::CNF);
                let rhs = rhs.to_set_direct(Mode::CNF);

                match op {
                    OpBinary::Or => {
                        let mut fm = Vec::with_capacity(lhs.sets.len() * rhs.sets.len());
                        for l_set in &lhs.sets {
                            for r_set in &rhs.sets {
                                let product = LiteralSet::from(
                                    l_set.literals().chain(r_set.literals()).cloned(),
                                );

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

impl<A: Atomic> FormulaSet<A> {
    pub fn filter_trivial_cnf(&mut self) {
        let mut set_index = 0;
        let mut set_limit = self.sets.len();

        'set_loop: while set_index < set_limit {
            self.sets[set_index].sort();
            for literal_index in 1..self.sets[set_index].len() {
                if self.sets[set_index].atom_at(literal_index - 1).id()
                    == self.sets[set_index].atom_at(literal_index).id()
                {
                    self.sets.swap_remove(set_index);
                    set_limit -= 1;
                    continue 'set_loop;
                }
            }
            set_index += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{formula_set::Mode, parse_propositional};

    #[test]
    fn cnf_true_false() {
        let t = parse_propositional("true");
        assert!(t.to_set_direct(Mode::CNF).is_cnf_top());

        let f = parse_propositional("false");
        assert!(f.to_set_direct(Mode::CNF).is_cnf_bot());
    }

    #[test]
    fn cnf_set() {
        let p_q = parse_propositional("p | r");
        let mut p_q_set = p_q.to_set_direct(Mode::CNF);

        let expr = parse_propositional("~p => r");
        let mut cnf = expr.to_cnf_formula_set_tseytin();
        cnf.one_literal_rule();

        cnf.sort_outer_and_inner();
        p_q_set.sort_outer_and_inner();
        assert_eq!(cnf, p_q_set)
    }
}
