use crate::logic::{
    Atomic, Formula, Literal, OpBinary, OpUnary,
    formula_set::{FormulaSet, LiteralSet, Mode, setify},
};

impl<A: Atomic> Formula<A> {
    pub(super) fn to_dnf_set_local(&self) -> Vec<LiteralSet<A>> {
        match self {
            Formula::True => vec![LiteralSet::default()],
            Formula::False => vec![],

            Formula::Atom(atom) => vec![LiteralSet {
                set: vec![Literal::from(atom.clone(), true)],
            }],

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => {
                    if let Formula::Atom(atom) = expr.as_ref() {
                        vec![LiteralSet {
                            set: vec![Literal::from(atom.clone(), false)],
                        }]
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
                        let mut fm = Vec::with_capacity(lhs.sets.len() * rhs.sets.len());
                        for l_set in &lhs.sets {
                            for r_set in &rhs.sets {
                                let mut product =
                                    l_set.set.iter().chain(r_set.set.iter()).cloned().collect();
                                setify(&mut product);
                                fm.push(LiteralSet { set: product });
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
        use std::cmp::Ordering::*;

        let mut limit = self.sets.len();
        let mut set_idx = 0;

        'set_loop: while set_idx < limit {
            if self.sets[set_idx].len() > 1 {
                match &self.sets[set_idx].get_negative_positive_split_index() {
                    Some(index) => index,
                    None => continue 'set_loop,
                };

                let (p, n) = self.sets[set_idx].get_negative_positive_splits();

                let mut p_index = 0;
                let mut n_index = 0;

                while p_index < p.len() && n_index < n.len() {
                    match p[p_index].atom().cmp(n[n_index].atom()) {
                        Less => p_index += 1,
                        Equal => {
                            self.sets.swap_remove(set_idx);
                            limit -= 1;
                            continue 'set_loop;
                        }
                        Greater => n_index += 1,
                    }
                }
            }
            set_idx += 1;
        }

        self.sets.sort_by(|a, b| a.cmp(b));
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
            for (literal_idx, literal) in base_set.set.iter().enumerate() {
                if literal != &self.sets[set_idx + 1].set[literal_idx] {
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
            [conjunction] => FormulaSet::literal_set_to_formula(OpBinary::And, conjunction),
            [first, remaining @ ..] => {
                let mut formula = FormulaSet::literal_set_to_formula(OpBinary::And, first);
                for other in remaining {
                    formula = Formula::Or(
                        formula,
                        FormulaSet::literal_set_to_formula(OpBinary::And, other),
                    )
                }
                formula
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, formula_set::Mode, parse_propositional};

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
        let dnf = expr.to_set_direct(Mode::DNF);
        let fm = dnf.as_formula();

        assert!(Formula::Iff(expr, fm).is_tautology());
    }
}
