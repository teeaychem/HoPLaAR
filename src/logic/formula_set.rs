use crate::logic::{Literal, Atomic, Formula, OpBinary, OpUnary};

// Invariant: Literals are sorted by `literal_cmp`.
type LiteralSet<A> = Vec<Literal<A>>;

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    CNF,
    DNF,
}

// A formula, as a set of sets.
// Invariant: `formula` is sorted by `literal_set_cmp`.
#[derive(Debug)]
pub struct FormulaSet<A: Atomic> {
    sets: Vec<LiteralSet<A>>,
    mode: Mode,
}

fn literal_set_cmp<A: Atomic>(a: &LiteralSet<A>, b: &LiteralSet<A>) -> std::cmp::Ordering {
    use std::cmp::Ordering::*;

    if a.is_empty() {
        return Less;
    }
    if b.is_empty() {
        return Greater;
    }

    let limit = std::cmp::min(a.len(), b.len());
    for idx in 0..limit {
        match a[idx].cmp(&b[idx]) {
            Less => return Less,
            Greater => return Greater,
            Equal => continue,
        }
    }

    a.len().cmp(&b.len())
}

fn literal_set_to_formula<A: Atomic>(op: OpBinary, ls: &LiteralSet<A>) -> Formula<A> {
    match ls.as_slice() {
        [] => Formula::True,
        [literal] => literal.as_formula(),
        [first, remaining @ ..] => {
            let mut formula = first.as_formula();
            for other in remaining {
                formula = Formula::Binary(op, formula, other.as_formula());
            }
            formula
        }
    }
}

impl<A: Atomic> std::fmt::Display for FormulaSet<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let outer_limit = self.sets.len().saturating_sub(1);

        let _ = write!(f, "{{");
        for (outer_idx, expr) in self.sets.iter().enumerate() {
            let inner_limit = expr.len().saturating_sub(1);

            let _ = write!(f, "{{");
            for (inner_idx, literal) in expr.iter().enumerate() {
                let _ = write!(f, "{literal}");
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
    pub fn sets(&self) -> &Vec<LiteralSet<A>> {
        &self.sets
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

    pub fn subsume(&mut self) {
        match self.mode {
            Mode::CNF => todo!(),
            Mode::DNF => self.dnf_subsume(),
        }
    }

    pub fn as_formula(&self) -> Formula<A> {
        match self.mode {
            Mode::CNF => self.cnf_formula(),
            Mode::DNF => self.dnf_formula(),
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

    pub fn dnf_is_bot(&self) -> bool {
        self.sets.is_empty()
    }

    pub fn dnf_is_top(&self) -> bool {
        self.sets.first().is_some_and(|set| set.is_empty())
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

impl<A: Atomic> FormulaSet<A> {
    pub fn cnf_is_bot(&self) -> bool {
        self.sets.first().is_some_and(|set| set.is_empty())
    }

    pub fn cnf_is_top(&self) -> bool {
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
    pub fn dnf_to_formula_set(&self) -> FormulaSet<A> {
        let mut formula = self.dnf_to_set();
        formula.sort_by(|a, b| literal_set_cmp(a, b));
        formula.dedup();

        FormulaSet {
            sets: formula,
            mode: Mode::DNF,
        }
    }

    fn dnf_to_set(&self) -> Vec<Vec<Literal<A>>> {
        match self {
            Formula::True => vec![vec![]],
            Formula::False => vec![],

            Formula::Atom { var } => vec![vec![Literal::from(var.clone(), true)]],

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => {
                    if let Formula::Atom { var } = expr.as_ref() {
                        vec![vec![Literal::from(var.clone(), false)]]
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

            Formula::Quantifier { .. } => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, parse_propositional_formula};

    #[test]
    fn dnf() {
        let expr = parse_propositional_formula("(p | q & r) & (~p | ~r)");
        let mut dnf = expr.dnf_to_formula_set();
        assert_eq!(dnf.sets().len(), 4);

        dnf.dnf_filter_contradictions();
        assert_eq!(dnf.sets().len(), 2);

        let expr = parse_propositional_formula("(p | p & r)");
        let dnf = expr.dnf_to_formula_set();
        assert_eq!(dnf.sets().len(), 2);

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

    #[test]
    fn dnf_subsumption() {
        let expr = parse_propositional_formula("(p | p & r) & true");
        let mut dnf = expr.dnf_to_formula_set();
        dnf.subsume();
        assert!(dnf.sets().first().is_some_and(|set| set.len() == 2));
    }

    #[test]
    fn dnf_formula() {
        let expr = parse_propositional_formula("(p | q & r) & (~p | ~r)");
        let dnf = expr.dnf_to_formula_set();
        let fm = dnf.as_formula();
        assert!(Formula::Iff(expr, fm).tautology());
    }
}
