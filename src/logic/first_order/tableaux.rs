use std::collections::VecDeque;

use crate::logic::{
    Formula, Literal, OpBinary,
    first_order::{
        FirstOrderFormula, Relation, Term, syntax::Substitution, terms::Var, unification::Unifier,
    },
    formula_set::{LiteralSet, Mode},
};

impl FirstOrderFormula {
    pub fn prawitz(&self, limit: Option<usize>) -> (bool, usize) {
        let base_clone = self.clone();
        let generalized = base_clone.generalize();
        let negated = generalized.negate();
        let skolemized = negated.skolemize();
        let mut base = skolemized.simple_dnf().to_set_direct(Mode::DNF);

        let mut unifier = Unifier::default();

        let limit = limit.unwrap_or(usize::MAX);

        let v_increment = std::cmp::max(
            1,
            base.variable_set()
                .iter()
                .map(|v| v.variant)
                .max()
                .unwrap_or_default(),
        );

        let increment_var = |var: &mut Var| var.variant += v_increment;

        let mut fm = base.clone();

        for attempt in 0..limit {
            if unifier.unify_refute(&fm) {
                return (true, attempt);
            }
            base.on_variables(increment_var);
            fm = fm.dnf_conjoin(base.clone());
        }

        (false, limit)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TableauOk {
    Refuted(usize),
    NoRefutation,
    InstantiationLimit,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TableauErr {
    Form,
    VariantsExhausted,
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

                    Formula::Unary { op, fml: expr } => {
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

                    Formula::Quantified { q, var, fml: fm } => {
                        use crate::logic::Quantifier;
                        //
                        match q {
                            Quantifier::ForAll => {
                                if instantiation_limit.is_some_and(|l| variant_index == l) {
                                    return Ok(TableauOk::InstantiationLimit);
                                }

                                let fresh_var = Var {
                                    id: var.id.to_owned(),
                                    variant: match variant_index.try_into() {
                                        Ok(n) => n,
                                        Err(_) => return Err(TableauErr::VariantsExhausted),
                                    },
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

    pub fn split_tableaux(self, instantiation_limit: Option<usize>) -> bool {
        let sfm = self.generalize().negate().skolemize_basic().simple_dnf();
        println!("{sfm}");
        let split = sfm.split_on(OpBinary::Or);
        split.into_iter().all(|disjunct| {
            matches!(
                Unifier::default().tableau(disjunct, None),
                Ok(TableauOk::Refuted(_))
            )
        })
    }
}

#[cfg(test)]
mod prawitz_tests {

    use crate::logic::first_order::{FirstOrderFormula, library};

    #[test]
    fn p18() {
        let f = FirstOrderFormula::from(library::pelletier::P18);
        let (result, _) = f.prawitz(None);
        assert!(result)
    }

    #[test]
    fn p19() {
        let f = FirstOrderFormula::from(library::pelletier::P19);
        let (result, _) = f.prawitz(Some(4));
        assert!(result);
    }

    #[test]
    fn p20() {
        let f = FirstOrderFormula::from(library::pelletier::P20);
        let (result, _) = f.prawitz(None);
        assert!(result)
    }

    #[test]
    fn p24() {
        let f = FirstOrderFormula::from(library::pelletier::P24);
        let (result, _) = f.prawitz(Some(2));
        assert!(result)
    }

    #[ignore = "???"]
    #[test]
    fn p45() {
        let fm = FirstOrderFormula::from(library::pelletier::P45);
        let (result, _) = fm.prawitz(Some(10));
        assert!(result);
    }

    #[test]
    fn sat_1() {
        let fm = FirstOrderFormula::from(library::satisfiable::AxPxQx);
        let (result, _) = fm.prawitz(Some(5));
        assert!(!result);
    }

    #[test]
    fn sat_2() {
        let fm = FirstOrderFormula::from(library::satisfiable::AxAyPxQy);
        let (result, _) = fm.prawitz(Some(5));
        assert!(!result);
    }

    #[test]
    fn sat_3() {
        let fm = FirstOrderFormula::from(library::satisfiable::AxEyPxQx);
        let (result, _) = fm.prawitz(Some(5));
        assert!(!result);
    }
}

#[cfg(test)]
mod tableau_tests {
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
    fn pelletier_monadic_predicate() {
        test_pelletier!(P20);
        test_pelletier!(P21);
        test_pelletier!(P22);
        test_pelletier!(P23);
        test_pelletier!(P24);
        test_pelletier!(P25);
        test_pelletier!(P26);
        test_pelletier!(P27);
        // test_pelletier!(P28);
        test_pelletier!(P29);
        test_pelletier!(P30);
        test_pelletier!(P31);
        test_pelletier!(P32);
        test_pelletier!(P33);
        test_pelletier!(P34);
    }

    #[test]
    fn pelletier_full_predicate() {
        test_pelletier!(P35);
        test_pelletier!(P36);
        test_pelletier!(P37);
        test_pelletier!(P38);
        test_pelletier!(P39);
        test_pelletier!(P40);
        test_pelletier!(P41);
        test_pelletier!(P42);
        test_pelletier!(P43);
        test_pelletier!(P44);
        test_pelletier!(P45);
        test_pelletier!(P46);
        test_pelletier!(P47);
    }

    #[test]
    fn p18() {
        test_pelletier!(P18)
    }

    #[test]
    fn p34_split() {
        use crate::logic::first_order::{FirstOrderFormula, library::pelletier};
        assert!(FirstOrderFormula::from(pelletier::P34).split_tableaux(None));
    }

    #[test]
    fn ewd1062_split() {
        let ewd1062 = "[∀x. R(x,x) ∧ ∀x. ∀y. ∀z.(R(x,y) ∧ R(y,z) → R(x,z)) ∧ ∀x. ∀y. (R(f(x),y) ↔ R(x,g(y)))] → [∀x.∀y.(R(x,y) → R(f(x),f(y))) ∧ ∀x.∀y.(R(x,y) → R(g(x),g(y)))]";
        let fm = FirstOrderFormula::from(ewd1062);
        assert!(fm.split_tableaux(None));
    }

    #[test]
    fn satisfiable() {
        use crate::logic::first_order::{FirstOrderFormula, library::satisfiable::*};

        assert!(!matches!(
            FirstOrderFormula::from(AxPxQx).tableaux(None),
            Ok(TableauOk::Refuted(_))
        ));

        assert!(!matches!(
            FirstOrderFormula::from(AxAyPxQy).tableaux(None),
            Ok(TableauOk::Refuted(_))
        ));

        assert!(!matches!(
            FirstOrderFormula::from(AxEyPxQx).tableaux(None),
            Ok(TableauOk::Refuted(_))
        ));
    }
}
