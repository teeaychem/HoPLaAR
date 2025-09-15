use std::collections::HashMap;

use crate::logic::{
    Formula, OpUnary,
    first_order::{
        FirstOrderFormula, Relation, Term,
        terms::{Fun, Var},
    },
};

pub type EqsSlice = [(Term, Term)];
pub type EqsVec = Vec<(Term, Term)>;

/// A struct which handles the state of unification, and bundles methods for unification.
///
/// The unification environment is split into two parts.
/// - A mapping from variables to indices.
/// - A collection of indexed terms.
#[derive(Clone, Debug, Default)]
pub struct Unifier {
    // The split mapping requires two steps.
    // However:
    // - Terms can be freely mutated while retaining the ability to lookup mappings.
    // - Iteration through terms does not require inspection of empty buckets in a hash map, etc.
    //
    /// A mapping from variables to the position of a term in `indexed_terms`.
    term_indicies: HashMap<Var, usize>,

    /// A collection of indexed terms.
    indexed_terms: Vec<Term>,
}

/// The type of mapping, with respect to some background env.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MapType {
    Trivial,
    Fresh,
    Cyclic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnificationFailure {
    Impossible,
    Cyclic,
}

impl Unifier {
    /// Returns the type of mapping that would result from updating `self` to send instance of `x` to `t`.
    ///
    /// Corresponds to `istriv` (`is_trivial`) from the book.
    /// Updated to make the test for cyclic test explicit.
    pub fn get_map_type(&self, x: &Var, t: &Term) -> MapType {
        match t {
            Term::F(Fun { args, .. }) => {
                for arg in args {
                    match self.get_map_type(x, arg) {
                        MapType::Trivial | MapType::Cyclic => return MapType::Cyclic,
                        MapType::Fresh => {}
                    }
                }
                MapType::Fresh
            }

            Term::V(y) if x == y => MapType::Trivial,
            Term::V(y) => match self.get_index(y) {
                Some(index) => self.get_map_type(x, &self.indexed_terms[index]),
                None => MapType::Fresh,
            },
        }
    }

    /// Returns the index of variable `v` in the env.
    pub fn get_index(&self, v: &Var) -> Option<usize> {
        self.term_indicies.get(v).cloned()
    }

    /// Returns the term which variable `v` maps to in the unification environment.
    pub fn get_value(&self, v: &Var) -> Option<&Term> {
        match self.get_index(v) {
            Some(index) => Some(&self.indexed_terms[index]),
            None => None,
        }
    }
}

impl Unifier {
    /// Inserts a mapping from `v` to `t` into the unification environment.
    pub fn insert(&mut self, v: Var, t: Term) {
        self.term_indicies
            .insert(v.clone(), self.indexed_terms.len());
        self.indexed_terms.push(t);
    }

    /// Unifies a sequences of equals.
    ///
    /// An iterative variant of a recursive implementation from the book.
    pub fn unify(&mut self, eqs: &EqsSlice) -> Result<(), UnificationFailure> {
        let mut todo = eqs.to_vec();

        while let Some((lhs, rhs)) = todo.pop() {
            match (lhs, rhs) {
                (Term::F(f), Term::F(g)) => {
                    if f == g {
                        todo.extend(f.args.iter().cloned().zip(g.args.iter().cloned()));
                    } else {
                        return Err(UnificationFailure::Impossible);
                    }
                }

                (Term::V(x), t) | (t, Term::V(x)) => {
                    if let Some(y) = self.get_value(&x) {
                        todo.push((y.clone(), t));
                    } else {
                        match self.get_map_type(&x, &t) {
                            MapType::Trivial => {}
                            MapType::Fresh => {
                                self.insert(x, t);
                            }
                            MapType::Cyclic => return Err(UnificationFailure::Cyclic),
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Updates the given term `t` by replacing variables with a term mapped by the unification environment, if possible.
    /// Otherwise, returns `t`.
    pub fn update_term(&mut self, t: Term) -> (Term, bool) {
        let mut update = false;

        match t {
            Term::F(mut fun) => {
                for arg in fun.args.iter_mut() {
                    let taken_arg = std::mem::take(arg);
                    match self.update_term(taken_arg) {
                        (t, true) => {
                            update = true;
                            *arg = t
                        }
                        (t, false) => *arg = t,
                    }
                }

                (Term::F(fun), update)
            }
            Term::V(ref var) => match self.get_value(var) {
                Some(y) => (y.clone(), true),
                None => (t, false),
            },
        }
    }

    /// Takes a single pass over the unification environment, updating each term mapped to, whenever possible.
    pub fn update_env_one_pass(&mut self) -> bool {
        let mut update = false;

        for index in 0..self.indexed_terms.len() {
            let to = std::mem::take(&mut self.indexed_terms[index]);
            match self.update_term(to) {
                (t, true) => {
                    update = true;
                    self.indexed_terms[index] = t
                }
                (t, false) => self.indexed_terms[index] = t,
            }
        }

        update
    }

    /// Solves a unification environment by repeatedly updating mapped to terms until a fixed point is established.
    /// A count of update passes until the fixed point is returned.
    pub fn solve(&mut self) -> usize {
        let mut passes = 0;
        while self.update_env_one_pass() {
            passes += 1;
        }
        passes
    }

    /// Fully unifies a seques of equals.
    pub fn fully_unify(&mut self, eqs: &EqsSlice) -> Result<(), UnificationFailure> {
        self.unify(eqs)?;
        self.solve();
        Ok(())
    }

    pub fn unify_and_apply(&mut self, eqs: &mut EqsVec) -> Result<(), UnificationFailure> {
        self.fully_unify(eqs)?;
        for (a, b) in eqs {
            let taken_a = std::mem::take(a);
            *a = self.update_term(taken_a).0;

            let taken_b = std::mem::take(b);
            *b = self.update_term(taken_b).0;
        }
        Ok(())
    }

    pub fn unify_literals(
        &mut self,
        l: &FirstOrderFormula,
        r: &FirstOrderFormula,
    ) -> Result<(), UnificationFailure> {
        use {Formula::*, OpUnary::*};
        match (l, r) {
            (Atom(Relation { terms: t_l, .. }), Atom(Relation { terms: t_r, .. })) => {
                let eqs: Vec<_> = t_l.iter().cloned().zip(t_r.iter().cloned()).collect();
                self.unify(&eqs)
            }

            (Unary { op: Not, expr: l_e }, Unary { op: Not, expr: r_e }) => {
                self.unify_literals(l_e, r_e)
            }

            (False, False) => Ok(()),

            _ => panic!("Can't unify literals"),
        }
    }
}

impl std::fmt::Display for Unifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (v, index) in &self.term_indicies {
            writeln!(f, "{v} \t=>\t {}", self.indexed_terms[*index])?
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{
        first_order::{
            FirstOrderFormula, Relation, Term,
            unification::{UnificationFailure, Unifier},
        },
        formula_set::Mode,
    };

    #[test]
    fn simple_one() {
        let mut u = Unifier::default();

        let t1 = Term::try_from("f(x, g(y))").unwrap();
        let t2 = Term::try_from("f(f(z), w)").unwrap();

        let e1 = Term::try_from("f(f(z), g(y))").unwrap();

        let mut eqs = vec![(t1, t2)];
        let _ = u.unify_and_apply(&mut eqs);
        match eqs.as_slice() {
            [(a, b)] => {
                assert!(a.subterm_eq(&e1));
                assert!(b.subterm_eq(&e1));
            }

            _ => unreachable!(),
        }
    }

    #[test]
    fn simple_two() {
        let mut u = Unifier::default();

        let t1 = Term::try_from("f(x, y)").unwrap();
        let t2 = Term::try_from("f(y, x)").unwrap();

        let e1 = Term::try_from("f(x, x)").unwrap();
        let e2 = Term::try_from("f(y, y)").unwrap();

        let mut eqs = vec![(t1, t2)];
        let _ = u.unify_and_apply(&mut eqs);
        match eqs.as_slice() {
            [(a, b)] => {
                assert!(a.subterm_eq(&e1) || a.subterm_eq(&e2));
                assert!(b.subterm_eq(&e1) || b.subterm_eq(&e2));
            }

            _ => unreachable!(),
        }
    }

    #[test]
    fn simple_cyclic() {
        let mut u = Unifier::default();

        let t1 = Term::try_from("f(x, g(y))").unwrap();
        let t2 = Term::try_from("f(y, x))").unwrap();
        let mut eqs = vec![(t1, t2)];
        let result = u.unify_and_apply(&mut eqs);
        assert_eq!(result, Err(UnificationFailure::Cyclic))
    }

    #[test]
    fn large_unifier() {
        let mut u = Unifier::default();

        let t1 = Term::try_from("x_0").unwrap();
        let t2 = Term::try_from("f(x_1, x_1)").unwrap();

        let t3 = Term::try_from("x_1").unwrap();
        let t4 = Term::try_from("f(x_2, x_2)").unwrap();

        let t5 = Term::try_from("x_2").unwrap();
        let t6 = Term::try_from("f(x_3, x_3)").unwrap();

        let e1 =
            Term::try_from("f(f(f(x_3, x_3), f(x_3, x_3)), f(f(x_3, x_3), f(x_3, x_3)))").unwrap();
        let e2 = Term::try_from("f(f(x_3, x_3), f(x_3, x_3))").unwrap();
        let e3 = Term::try_from("f(x_3, x_3)").unwrap();

        let mut eqs = vec![(t1, t2), (t3, t4), (t5, t6)];
        let _ = u.unify_and_apply(&mut eqs);
        match eqs.as_slice() {
            [(a, b), (c, d), (e, f)] => {
                assert!(a.subterm_eq(&e1));
                assert!(b.subterm_eq(&e1));

                assert!(c.subterm_eq(&e2));
                assert!(d.subterm_eq(&e2));

                assert!(e.subterm_eq(&e3));
                assert!(f.subterm_eq(&e3));
            }

            _ => unreachable!(),
        }
    }

    #[test]
    fn debug_simple_two_relation() {
        let mut u = Unifier::default();

        let t1 = FirstOrderFormula::from("R(x, y)");
        let t2 = FirstOrderFormula::from("R(y, x)");

        let _ = u.unify_literals(&t1, &t2);

        println!("{u}");
    }
}
