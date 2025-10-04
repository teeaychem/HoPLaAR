use std::collections::VecDeque;

use crate::logic::{
    Formula, Literal, OpBinary,
    first_order::{FirstOrderFormula, Relation, unification::Unifier},
    formula_set::LiteralSet,
};

impl Unifier {
    pub fn tableau(&mut self, fms: FirstOrderFormula, level: usize) -> bool {
        let mut q: VecDeque<FirstOrderFormula> = VecDeque::default();
        let mut disjunct: LiteralSet<Relation> = LiteralSet::default();
        let mut branch_refuted: bool = false;

        struct Continuation {
            q: VecDeque<FirstOrderFormula>,
            s: LiteralSet<Relation>,
            trail_length: usize,
        }

        let mut stack: Vec<Continuation> = Vec::default();
        stack.push(Continuation {
            q: VecDeque::from([fms]),
            s: LiteralSet::default(),
            trail_length: 0,
        });

        use Formula::*;

        while let Some(todo) = stack.pop() {
            q = todo.q;
            disjunct = todo.s;
            let obsolete_unifications = self.trail.len() - todo.trail_length;
            self.pop_multiple(obsolete_unifications);

            'branch_loop: while let Some(head) = q.pop_front() {
                //
                match head {
                    True => todo!("true"),
                    False => todo!("false"),
                    Atom(relation) => {
                        disjunct.insert(Literal::from(relation, true));
                        match self.unify_complements(&disjunct) {
                            Ok(_) => {
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
                                    Ok(_) => {
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
                                q.push_front(*rhs);
                                q.push_front(*lhs);
                            }
                            OpBinary::Or => {
                                let mut tbc = Continuation {
                                    q: q.clone(),
                                    s: disjunct.clone(),
                                    trail_length: self.trail.len(),
                                };
                                tbc.q.push_front(*rhs);
                                stack.push(tbc);

                                q.push_front(*lhs);
                            }
                            OpBinary::Imp | OpBinary::Iff => todo!("NNF with implication"),
                        }
                    }
                    Quantified { q, var, fm } => todo!(),
                }
            }

            println!("Literals: {disjunct}");
            match branch_refuted {
                true => continue,
                false => return false,
            }
        }
        true
    }
}

fn tableaux_refute(fm: FirstOrderFormula) -> bool {
    let sfm = fm.generalize().negate().skolemize_basic();

    let mut u = Unifier::default();

    u.tableau(sfm, 0)
}

#[cfg(test)]
mod tests {
    use crate::logic::first_order::{FirstOrderFormula, library::pelletier::P38};

    use super::*;

    #[test]
    fn debug() {
        let fm = FirstOrderFormula::from(P38);

        let fm = FirstOrderFormula::from("(P(a) & P(b)) | (~P(a) & ~P(b))");
        let result = tableaux_refute(fm);
        println!("Result: {result}");
    }
}
