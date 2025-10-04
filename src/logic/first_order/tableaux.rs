use std::collections::VecDeque;

use crate::logic::{
    Formula, Literal, OpBinary,
    first_order::{
        FirstOrderFormula, Relation, Term, syntax::Substitution, terms::Var, unification::Unifier,
    },
    formula_set::LiteralSet,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TableauOk {
    Refuted(usize),
    NoRefutation,
    InstantiationLimit,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TableauErr {
    Form,
}

impl Unifier {
    /// An iterative varaint of tableau from the book.
    ///
    /// Continutations are replaced by a stack of custom continuation structs.
    /// When brancing a continutation is created, and when a branch completes a continuation is taken from the stack.
    pub fn tableau(
        &mut self,
        fms: FirstOrderFormula,
        instantiation_limit: Option<usize>,
    ) -> Result<TableauOk, TableauErr> {
        let mut formula_q: VecDeque<FirstOrderFormula> = VecDeque::default();
        let mut disjunct: LiteralSet<Relation> = LiteralSet::default();

        let mut check_disjunct: bool = false;
        let mut variant_index = 1;

        #[derive(Debug)]
        struct Continuation {
            queue: VecDeque<FirstOrderFormula>,
            disjunct: LiteralSet<Relation>,
            variant_index: usize, // Ensure the instantiation count is only wrt. a branch.
            trail_length: usize,  // Used to remove unifications from an explored branch.
        }

        let mut stack: Vec<Continuation> = Vec::default();
        stack.push(Continuation {
            queue: VecDeque::from([fms]),
            disjunct: LiteralSet::default(),
            variant_index: 1,
            trail_length: 0,
        });

        'disjunct_loop: while let Some(todo) = stack.pop() {
            formula_q = todo.queue;
            disjunct = todo.disjunct;
            variant_index = todo.variant_index;

            let obsolete_unifications = self.trail.len() - todo.trail_length;
            self.pop_multiple(obsolete_unifications);

            check_disjunct = false;

            'branch_loop: while let Some(head) = formula_q.pop_front() {
                //
                match head {
                    Formula::True | Formula::False => return Err(TableauErr::Form),

                    Formula::Atom(relation) => {
                        disjunct.insert(Literal::from(relation, true));
                        check_disjunct = true;
                    }

                    Formula::Unary { op, expr } => {
                        //
                        match *expr {
                            Formula::Atom(relation) => {
                                disjunct.insert(Literal::from(relation, false));
                                check_disjunct = true;
                            }
                            _ => return Err(TableauErr::Form),
                        }
                    }

                    Formula::Binary { op, lhs, rhs } => {
                        //
                        match op {
                            OpBinary::And => {
                                formula_q.push_front(*rhs);
                                formula_q.push_front(*lhs);
                            }
                            OpBinary::Or => {
                                let mut tbc = Continuation {
                                    queue: formula_q.clone(),
                                    disjunct: disjunct.clone(),
                                    variant_index,
                                    trail_length: self.trail.len(),
                                };
                                tbc.queue.push_front(*rhs);
                                stack.push(tbc);

                                formula_q.push_front(*lhs);
                            }
                            OpBinary::Imp | OpBinary::Iff => return Err(TableauErr::Form),
                        }
                    }

                    Formula::Quantified { q, var, fm } => {
                        use crate::logic::Quantifier;
                        //
                        match q {
                            Quantifier::ForAll => {
                                if instantiation_limit.is_some_and(|l| variant_index == l) {
                                    return Ok(TableauOk::InstantiationLimit);
                                }

                                let fresh_var = Var {
                                    id: var.id.to_owned(),
                                    variant: variant_index.try_into().unwrap(),
                                };

                                let mut v_substitution =
                                    Substitution::from_interrupt(&var, Some(Term::V(fresh_var)));

                                let sfm = fm.clone().term_substitution(&mut v_substitution);
                                formula_q.push_front(sfm);

                                formula_q.push_back(FirstOrderFormula::Quantified(q, var, *fm));
                                variant_index += 1;
                            }

                            Quantifier::Exists => return Err(TableauErr::Form),
                        }
                    }
                }

                if check_disjunct {
                    match self.unify_complements(&disjunct) {
                        Ok((_, _, _)) => continue 'disjunct_loop,
                        Err(_) => continue 'branch_loop,
                    }
                }
            }

            return Ok(TableauOk::NoRefutation);
        }

        Ok(TableauOk::Refuted(variant_index))
    }
}

impl FirstOrderFormula {
    pub fn tableaux(self, instantiation_limit: Option<usize>) -> Result<TableauOk, TableauErr> {
        let sfm = self.generalize().negate().skolemize_basic();

        let mut u = Unifier::default();

        u.tableau(sfm, instantiation_limit)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[macro_export]
    macro_rules! test_pelletier {
        (  $x:ident ) => {{
            use $crate::logic::first_order::{FirstOrderFormula, library::pelletier};
            let result = FirstOrderFormula::from(pelletier::$x).tableaux(None);
            assert!(matches!(result, Ok(TableauOk::Refuted(_))));
        }};
    }

    #[test]
    fn pelletier_propositional() {
        test_pelletier!(P1);
        test_pelletier!(P2);
        test_pelletier!(P3);
        test_pelletier!(P4);
        test_pelletier!(P5);
        test_pelletier!(P6);
        test_pelletier!(P7);
        test_pelletier!(P8);
        test_pelletier!(P9);
        test_pelletier!(P10);
        test_pelletier!(P11);
        test_pelletier!(P12);
        test_pelletier!(P13);
        test_pelletier!(P14);
        test_pelletier!(P15);
        test_pelletier!(P16);
        test_pelletier!(P17);
    }

    #[test]
    fn p18() {
        test_pelletier!(P18)
    }

    #[test]
    fn p38() {
        test_pelletier!(P38)
    }
}
