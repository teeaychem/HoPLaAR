use crate::logic::{Atomic, Formula, OpBinary, OpUnary};

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    CNF,
    DNF,
}

#[derive(Debug)]
pub struct FormulaSet<A: Atomic> {
    formula: Vec<Vec<(A, bool)>>,
    mode: Mode,
}

impl<A: Atomic> std::fmt::Display for FormulaSet<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let outer_limit = self.formula.len().saturating_sub(1);

        let _ = write!(f, "{{");
        for (outer_idx, expr) in self.formula.iter().enumerate() {
            let inner_limit = expr.len().saturating_sub(1);

            let _ = write!(f, "{{");
            for (inner_idx, (atom, value)) in expr.iter().enumerate() {
                let _ = match value {
                    true => write!(f, "{atom}"),
                    false => write!(f, "¬{atom}"),
                };
                if inner_idx < inner_limit {
                    let _ = write!(f, ", ");
                }
            }
            let _ = write!(f, "}}");
            if outer_idx < outer_limit {
                let _ = write!(f, ", ");
            }
        }
        let _ = write!(f, "}}");

        Ok(())
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn formula(&self) -> &Vec<Vec<(A, bool)>> {
        &self.formula
    }

    pub fn mode(&self) -> Mode {
        self.mode
    }

    pub fn filter_contradictions(&mut self) {
        match self.mode {
            Mode::CNF => todo!(),
            Mode::DNF => self.dnf_filter_contradictions(),
        }
    }

    pub fn is_bot(&self) -> bool {
        match self.mode {
            Mode::CNF => self.cnf_is_bot(),
            Mode::DNF => self.dnf_is_bot(),
        }
    }

    pub fn is_top(&self) -> bool {
        match self.mode {
            Mode::CNF => self.cnf_is_top(),
            Mode::DNF => self.dnf_is_top(),
        }
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn dnf_filter_contradictions(&mut self) {
        let mut limit = self.formula.len();
        let mut set_idx = 0;

        'set_loop: while set_idx < limit {
            if self.formula[set_idx].len() > 1 {
                for idx in 1..self.formula[set_idx].len() {
                    if self.formula[set_idx][idx - 1].0 == self.formula[set_idx][idx].0 {
                        self.formula.swap_remove(set_idx);
                        limit -= 1;
                        continue 'set_loop;
                    }
                }
            }

            set_idx += 1;
        }

        self.formula.sort();
    }

    pub fn dnf_is_bot(&self) -> bool {
        self.formula.is_empty()
    }

    pub fn dnf_is_top(&self) -> bool {
        self.formula.first().is_some_and(|set| set.is_empty())
    }
}

impl<A: Atomic> FormulaSet<A> {
    pub fn cnf_is_bot(&self) -> bool {
        self.formula.first().is_some_and(|set| set.is_empty())
    }

    pub fn cnf_is_top(&self) -> bool {
        self.formula.is_empty()
    }
}

impl<A: Atomic> Formula<A> {
    pub fn dnf_to_formula_set(&self) -> FormulaSet<A> {
        let mut formula = self.dnf_to_set();
        formula.sort();
        formula.dedup();

        FormulaSet {
            formula,
            mode: Mode::DNF,
        }
    }

    fn dnf_to_set(&self) -> Vec<Vec<(A, bool)>> {
        match self {
            Formula::True => vec![vec![]],
            Formula::False => vec![],

            Formula::Atom { var } => vec![vec![(var.clone(), true)]],

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => {
                    if let Formula::Atom { var } = expr.as_ref() {
                        vec![vec![(var.clone(), false)]]
                    } else {
                        panic!()
                    }
                }
            },

            Formula::Binary { op, lhs, rhs } => {
                let lhs = lhs.dnf_to_formula_set();
                let rhs = rhs.dnf_to_formula_set();

                match op {
                    OpBinary::And => {
                        let mut fm = Vec::with_capacity(lhs.formula.len() * rhs.formula.len());
                        for l_set in &lhs.formula {
                            for r_set in &rhs.formula {
                                let mut product: Vec<(A, bool)> =
                                    l_set.iter().chain(r_set).cloned().collect();

                                // 'Setify'
                                product.sort(); // As the product is partially sorted, stable sort is preferred.
                                product.dedup();

                                fm.push(product);
                            }
                        }
                        fm
                    }

                    OpBinary::Or => lhs.formula.into_iter().chain(rhs.formula).collect(),

                    OpBinary::Imp | OpBinary::Iff => panic!(),
                }
            }

            Formula::Quantifier { .. } => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::parse_propositional_formula;

    #[test]
    pub fn dnf() {
        let expr = parse_propositional_formula("(p | q & r) & (~p | ~r)");
        let mut dnf = expr.dnf_to_formula_set();
        assert_eq!(dnf.formula().len(), 4);

        dnf.dnf_filter_contradictions();
        assert_eq!(dnf.formula().len(), 2);

        let expr = parse_propositional_formula("(p | q & r) & true");
        let dnf = expr.dnf_to_formula_set();
        assert_eq!(dnf.formula().len(), 2);

        let expr = parse_propositional_formula("false");
        let dnf = expr.dnf_to_formula_set();
        assert!(dnf.is_bot());

        let expr = parse_propositional_formula("p & ~p");
        let mut dnf = expr.dnf_to_formula_set();
        dnf.filter_contradictions();
        assert!(dnf.is_bot());

        let expr = parse_propositional_formula("true");
        let dnf = expr.dnf_to_formula_set();
        assert!(dnf.is_top());
    }
}
