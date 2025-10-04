use std::collections::VecDeque;

use crate::logic::{
    Formula, Literal, OpBinary,
    first_order::{FirstOrderFormula, Relation, Term, syntax::Substitution, unification::Unifier},
    formula_set::LiteralSet,
};

#[derive(Debug)]
pub enum TableauResult {
    Refuted(usize),
    NoRefutation,
    InstantiationLimit,
}

impl Unifier {
    pub fn tableau(&mut self, fms: FirstOrderFormula, level: usize) -> Result<TableauResult, ()> {
        let mut formula_q: VecDeque<FirstOrderFormula> = VecDeque::default();
        let mut disjunct: LiteralSet<Relation> = LiteralSet::default();

        let mut branch_refuted: bool = false;
        let mut k = 1;

        #[derive(Debug)]
        struct Continuation {
            q: VecDeque<FirstOrderFormula>,
            s: LiteralSet<Relation>,
            instantiations: usize,
            trail_length: usize,
        }

        let mut stack: Vec<Continuation> = Vec::default();
        stack.push(Continuation {
            q: VecDeque::from([fms]),
            s: LiteralSet::default(),
            instantiations: 1,
            trail_length: 0,
        });

        use Formula::*;

        'disjunct_loop: while let Some(todo) = stack.pop() {
            formula_q = todo.q;
            disjunct = todo.s;
            k = todo.instantiations;

            let obsolete_unifications = self.trail.len() - todo.trail_length;
            self.pop_multiple(obsolete_unifications);

            branch_refuted = false;

            'branch_loop: while let Some(head) = formula_q.pop_front() {
                //
                match head {
                    True => todo!("true"),
                    False => todo!("false"),

                    Atom(relation) => {
                        disjunct.insert(Literal::from(relation, true));
                        match self.unify_complements(&disjunct) {
                            Ok((_, _, unifications)) => {
                                branch_refuted = true;
                                break 'branch_loop;
                            }
                            Err(_) => continue,
                        }
                    }

                    Unary { op, expr } => {
                        //
                        match *expr {
                            Atom(relation) => {
                                disjunct.insert(Literal::from(relation, false));
                                match self.unify_complements(&disjunct) {
                                    Ok((_, _, unifications)) => {
                                        branch_refuted = true;
                                        break 'branch_loop;
                                    }
                                    Err(_) => continue,
                                }
                            }
                            _ => panic!("Complex negation"),
                        }
                    }

                    Binary { op, lhs, rhs } => {
                        //
                        match op {
                            OpBinary::And => {
                                formula_q.push_front(*rhs);
                                formula_q.push_front(*lhs);
                            }
                            OpBinary::Or => {
                                let mut tbc = Continuation {
                                    q: formula_q.clone(),
                                    s: disjunct.clone(),
                                    instantiations: k,
                                    trail_length: self.trail.len(),
                                };
                                tbc.q.push_front(*rhs);
                                stack.push(tbc);

                                formula_q.push_front(*lhs);
                            }
                            OpBinary::Imp | OpBinary::Iff => todo!("NNF with implication"),
                        }
                    }

                    Quantified { q, var, fm } => {
                        use crate::logic::Quantifier::*;
                        //
                        match q {
                            ForAll => {
                                if k == level {
                                    return Ok(TableauResult::InstantiationLimit);
                                }
                                let c = Term::Var(&format!("{}_{}", var, k));

                                let mut s = Substitution::default();
                                s.add_interrupt(&var, Some(c));

                                let fms = fm.clone().term_substitution(&mut s);
                                formula_q.push_front(fms);

                                formula_q.push_back(FirstOrderFormula::Quantified(q, var, *fm));
                                k += 1;
                            }

                            Exists => panic!("Existential in NNF"),
                        }
                    }
                }
            }

            match branch_refuted {
                true => {
                    println!("Refuted {disjunct}");
                    continue 'disjunct_loop;
                }
                false => return Ok(TableauResult::NoRefutation),
            }
        }

        Ok(TableauResult::Refuted(k))
    }
}

fn tableaux(fm: FirstOrderFormula) -> Result<TableauResult, ()> {
    let sfm = fm.generalize().negate().skolemize_basic();

    let mut u = Unifier::default();

    u.tableau(sfm, 10)
}

#[cfg(test)]
mod tests {
    use crate::logic::first_order::{FirstOrderFormula, library::pelletier::*};

    use super::*;

    #[test]
    fn debug() {
        // let fm = FirstOrderFormula::from(P38);

        // let fm = FirstOrderFormula::from("(P(a) & P(b)) | (~P(a) & ~P(b))");
        // let result = tableaux_refute(fm);
        // println!("Result: {result:?}");

        // let fm = FirstOrderFormula::from("exists x. (P(x) | ~P(x))");
        // let result = tableaux_refute(fm);
        // println!("Result: {result:?}");

        let fm = FirstOrderFormula::from(P38);
        // let fm = FirstOrderFormula::from(
        //     "forall x. ((P(x) & forall y. (Q(y) | ~Q(y))) | (~P(x) & forall y. (Q(y) | ~Q(y))))",
        // );
        let result = tableaux(fm);
        println!("Result: {result:?}");
    }
}
