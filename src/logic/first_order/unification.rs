use std::collections::HashMap;

use crate::logic::{
    Formula, OpUnary,
    first_order::{
        FirstOrderFormula, Relation, Term,
        terms::{Fun, Var},
    },
    formula_set::{FormulaSet, LiteralSet, Mode},
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
    /// A mapping from variables to a term.
    pub var_to_term: HashMap<Var, Term>,
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
    Distinct,
    FormulaMismatch,
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
            Term::V(y) => match self.get_term(y) {
                Some(t) => self.get_map_type(x, t),
                None => MapType::Fresh,
            },
        }
    }

    /// Returns the term which variable `v` maps to in the unification environment.
    pub fn get_term(&self, v: &Var) -> Option<&Term> {
        self.var_to_term.get(v)
    }
}

impl Unifier {
    /// Inserts a mapping from `v` to `t` into the unification environment.
    pub fn insert(&mut self, v: Var, t: Term) -> Option<Term> {
        self.var_to_term.insert(v, t)
    }

    pub fn clear(&mut self) {
        self.var_to_term.clear();
    }

    /// Unifies a sequences of equals.
    ///
    /// An iterative variant of a recursive implementation from the book.
    pub fn unify(&mut self, eqs: &EqsSlice) -> Result<(), UnificationFailure> {
        let mut todo = eqs.to_vec();

        while let Some((lhs, rhs)) = todo.pop() {
            match (lhs, rhs) {
                (Term::F(f), Term::F(g)) => {
                    use std::cmp::Ordering::*;
                    match f.cmp(&g) {
                        Equal => todo.extend(f.args.iter().cloned().zip(g.args.iter().cloned())),
                        _ => {
                            return Err(UnificationFailure::Distinct);
                        }
                    }
                }

                (Term::V(x), t) | (t, Term::V(x)) => {
                    if let Some(y) = self.get_term(&x) {
                        todo.push((y.clone(), t));
                    } else {
                        use MapType::*;
                        match self.get_map_type(&x, &t) {
                            Trivial => {}
                            Fresh => {
                                self.insert(x, t);
                            }
                            Cyclic => return Err(UnificationFailure::Cyclic),
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Updates the given term `t` by replacing variables with a term mapped by the unification environment, if possible.
    /// Otherwise, returns `t`.
    pub fn update_term(vt: &HashMap<Var, Term>, t: Term) -> (Term, bool) {
        let mut update = false;

        match t {
            Term::F(mut fun) => {
                for arg in fun.args.iter_mut() {
                    let taken_arg = std::mem::take(arg);
                    match Self::update_term(vt, taken_arg) {
                        (t, true) => {
                            update = true;
                            *arg = t
                        }
                        (t, false) => *arg = t,
                    }
                }

                (Term::F(fun), update)
            }
            Term::V(ref var) => match vt.get(var) {
                Some(y) => (y.clone(), true),
                None => (t, false),
            },
        }
    }

    /// Takes a single pass over the unification environment, updating each term mapped to, whenever possible.
    pub fn update_env_one_pass(&mut self) -> bool {
        let mut update = false;

        // TODO: A better way.
        let variables: Vec<_> = self.var_to_term.keys().cloned().collect();

        for variable in variables {
            let term = self.var_to_term.get(&variable).unwrap().clone();

            let (term, fresh) = Self::update_term(&self.var_to_term, term);

            if fresh {
                update = true;
                self.var_to_term.insert(variable, term);
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
            *a = Self::update_term(&self.var_to_term, taken_a).0;

            let taken_b = std::mem::take(b);
            *b = Self::update_term(&self.var_to_term, taken_b).0;
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
            (Atom(l), Atom(r)) => self.unify_relations(l, r),

            (Unary { op: Not, expr: l_e }, Unary { op: Not, expr: r_e }) => {
                self.unify_literals(l_e, r_e)
            }

            (False, False) => Ok(()),

            _ => Err(UnificationFailure::FormulaMismatch),
        }
    }

    /// Extends `self` with a unification of relations `l` and `r`, if possible.
    pub fn unify_relations(
        &mut self,
        l: &Relation,
        r: &Relation,
    ) -> Result<(), UnificationFailure> {
        match l.id == r.id && l.terms.len() == r.terms.len() {
            true => {
                let l_terms = l.terms.iter().cloned();
                let r_terms = r.terms.iter().cloned();
                let eqs: Vec<_> = l_terms.zip(r_terms).collect();
                self.unify(&eqs)
            }

            false => Err(UnificationFailure::Distinct),
        }
    }
}

impl Unifier {
    /// Searches for a pair of complementary literals.
    /// Returns true on the first unifier found, with `self` is updated with the unifier
    /// Returns false, otherwise.
    pub fn unify_complements(&mut self, set: &LiteralSet<Relation>) -> bool {
        // Splits the set into positive and negative literals, then examines all possible complements.

        let (n, p) = set.negative_positive_split();

        if n.is_empty() || p.is_empty() {
            return false;
        }

        for nx in n {
            for px in p {
                if let Ok(()) = self.unify_relations(nx.atom(), px.atom()) {
                    return true;
                }
            }
        }

        false
    }

    /// Attemps to extend `self` with a unifier for a pair of complementary literals for each set of relations in `fs`.
    /// Returns true if a unifier for complementary literals has been found for each set of `fs`.
    /// Returns false otherwise --- specifically, immediately on finding a set for which no unifier is available.
    pub fn unify_refute(&mut self, fs: &FormulaSet<Relation>) -> bool {
        for disjunct in fs.sets().iter() {
            match self.unify_complements(disjunct) {
                true => {}
                false => return false,
            }
        }
        true
    }
}

impl std::fmt::Display for Unifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (v, t) in &self.var_to_term {
            writeln!(f, "{v} \t=>\t {t}")?
        }

        Ok(())
    }
}

// etc.

impl FirstOrderFormula {
    pub fn prawitz(&self, limit: Option<usize>) -> (bool, usize) {
        let base_clone = self.clone();
        let generalized = base_clone.generalize();
        let negated = generalized.negate();
        let skolemized = negated.skolemize();
        let mut base = skolemized.raw_dnf().to_set_direct(Mode::DNF);

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

#[cfg(test)]
mod tests {
    use crate::logic::{
        first_order::{
            FirstOrderFormula, Term,
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

        println!("{u}");

        match eqs.as_slice() {
            [(a, b), (c, d), (e, f)] => {
                println!("{a} | {e1}");
                println!("{b} | {e1}");
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

    #[test]
    fn udebug() {
        // let mut fm = FirstOrderFormula::from("exists x. (P(x) & ~P(x))");
        let mut fm = FirstOrderFormula::from("forall x. (P(x) | ~P(x))");
        fm = fm.generalize().negate().skolemize().raw_dnf();

        println!("{fm}");

        let fms = fm.to_set_direct(Mode::DNF);
        println!("{fms}");

        let mut u = Unifier::default();
        let result = u.unify_refute(&fms);

        println!("{result:?}");
        println!("Unified complements: {u}");
    }
}

#[cfg(test)]
mod formula_tests {

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
        let (result, _) = f.prawitz(None);

        assert!(result)
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
        let (result, _) = f.prawitz(None);

        assert!(result)
    }

    #[ignore = "Unsatisfiability test too inefficient"]
    #[test]
    fn p45() {
        let fm = FirstOrderFormula::from(library::pelletier::P45);
        let (result, _) = fm.prawitz(Some(10));

        assert!(result);
    }
}
