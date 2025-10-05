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
pub type VarTermMap = std::collections::HashMap<Var, Term>;

/// A struct which handles the state of unification, and bundles methods for unification.
///
/// The unification environment is split into two parts.
/// - A mapping from variables to indices.
/// - A collection of indexed terms.
#[derive(Clone, Debug, Default)]
pub struct Unifier {
    /// A mapping from variables to a term.
    pub var_to_term: VarTermMap,

    pub trail: Vec<Var>,
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
        self.trail.push(v.clone());
        self.var_to_term.insert(v, t)
    }

    pub fn pop(&mut self) -> Option<Term> {
        match self.trail.pop() {
            Some(var) => self.var_to_term.remove(&var),
            None => None,
        }
    }

    pub fn pop_multiple(&mut self, count: usize) {
        for _ in 0..count {
            let var = self.trail.pop().unwrap();
            self.var_to_term.remove(&var);
        }
    }

    /// Unifies a sequences of equals.
    ///
    /// An iterative variant of a recursive implementation from the book.
    pub fn unify(&mut self, eqs: &EqsSlice) -> Result<usize, UnificationFailure> {
        let mut todo = eqs.to_vec();
        // Store a count of unification made in order to undo additions on failure.
        // And, to return a count of unifications made.
        let mut fresh_unifications = 0;

        while let Some((lhs, rhs)) = todo.pop() {
            match (lhs, rhs) {
                (Term::F(f), Term::F(g)) => match f.cmp(&g) {
                    std::cmp::Ordering::Equal => {
                        todo.extend(f.args.iter().cloned().zip(g.args.iter().cloned()).rev())
                    }
                    _ => {
                        self.pop_multiple(fresh_unifications);
                        return Err(UnificationFailure::Distinct);
                    }
                },

                (Term::V(x), t) | (t, Term::V(x)) => {
                    if let Some(y) = self.get_term(&x) {
                        todo.push((y.clone(), t));
                    } else {
                        match self.get_map_type(&x, &t) {
                            MapType::Trivial => {}
                            MapType::Fresh => {
                                self.insert(x, t);
                                fresh_unifications += 1;
                            }
                            MapType::Cyclic => {
                                self.pop_multiple(fresh_unifications);
                                return Err(UnificationFailure::Cyclic);
                            }
                        }
                    }
                }
            }
        }

        Ok(fresh_unifications)
    }

    /// Updates the given term `t` by replacing variables with a term mapped by the unification environment, if possible.
    /// Otherwise, returns `t`.
    pub fn update_term(vt: &VarTermMap, t: Term) -> (Term, bool) {
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
    pub fn flatten_once(&mut self) -> bool {
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

        // TODO: Establish whether this is required.
        // The issue is whether an earlier variable may now map to a later term.
        // If so, there's no way to remove unifications.
        if update {
            self.trail.clear();
        }

        update
    }

    /// Solves a unification environment by repeatedly updating mapped to terms until a fixed point is established.
    /// A count of update passes until the fixed point is returned.
    pub fn solve(&mut self) -> usize {
        let mut passes = 0;
        while self.flatten_once() {
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
    ) -> Result<usize, UnificationFailure> {
        use {Formula::*, OpUnary::*};
        match (l, r) {
            (Atom(l), Atom(r)) => self.unify_relations(l, r),

            (Unary { op: Not, expr: l_e }, Unary { op: Not, expr: r_e }) => {
                self.unify_literals(l_e, r_e)
            }

            (False, False) => Ok(0),

            _ => Err(UnificationFailure::FormulaMismatch),
        }
    }

    /// Extends `self` with a unification of relations `l` and `r`, if possible.
    pub fn unify_relations(
        &mut self,
        l: &Relation,
        r: &Relation,
    ) -> Result<usize, UnificationFailure> {
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
    /// Applies `self` to `relation`, returning a fresh relation.
    pub fn apply_to_relation(&self, relation: &Relation) -> Relation {
        Relation {
            id: relation.id.clone(),
            terms: relation
                .terms
                .iter()
                .map(|t| Self::update_term(&self.var_to_term, t.clone()).0)
                .collect(),
        }
    }

    /// Searches for a pair of complementary literals in disjunct at `disjunct_idx` of `fs`.
    ///
    /// The search starts from the negative literal at index `nl_idx`.
    /// And, positive literal at `pl_idx`, respectively.
    ///
    /// If a complementary pair is found at tuple is returned of the form:
    /// (current negative literal index, current positive literal index, fresh unifiers)
    /// With this the search can be restarted from the same place without the fresh unifiers.
    ///
    /// If the set contains complementary literals without the need to unify, the current indicies
    /// are set to their respectively limits, as for intended applications there would be no
    /// need to restart the search.
    ///
    /// Returns a unit error if no unification is possible.
    pub fn unify_complements_custom(
        &mut self,
        disjunct: &LiteralSet<Relation>,
        mut nl_idx: usize,
        mut pl_idx: usize,
    ) -> Result<(usize, usize, usize), ()> {
        // Splits the set into positive and negative literals, then examines all possible complements.

        let (n, p) = disjunct.negative_positive_split();

        while nl_idx < n.len() {
            while pl_idx < p.len() {
                // If the relations are the same, investigate...
                if n[nl_idx].atom().id == p[pl_idx].atom().id {
                    // To check if the relations are in conflict, apply the current unifier.
                    let nlu = self.apply_to_relation(n[nl_idx].atom());
                    let plu = self.apply_to_relation(p[pl_idx].atom());

                    if nlu == plu {
                        // The literals are complementary given the current unification.
                        // So, there's no reason to consider the set any further.
                        return Ok((n.len(), p.len(), 0));
                    } else {
                        // The literals are not complementary, so attempt unification.
                        match self.unify_relations(n[nl_idx].atom(), p[pl_idx].atom()) {
                            Ok(0) | Err(_) => {} // As no unifier was found, continue the refutation search.
                            Ok(fresh) => {
                                // The unifier has been expanded.
                                // Every terms pair was either trivial or freshly mapped.
                                // So, a conflict has been found, so continue.
                                return Ok((nl_idx, pl_idx, fresh));
                            }
                        }
                    }
                }

                pl_idx += 1;
            }
            pl_idx = 0;
            nl_idx += 1;
        }

        Err(())
    }

    pub fn unify_complements(
        &mut self,
        disjunct: &LiteralSet<Relation>,
    ) -> Result<(usize, usize, usize), ()> {
        self.unify_complements_custom(disjunct, 0, 0)
    }

    // Quite inefficient, as the same unification may be explored multiple (multiple) times.
    #[allow(clippy::single_match)]
    fn unify_refute(&mut self, fs: &FormulaSet<Relation>) -> bool {
        // A stack to emulate a recursive search.
        // Stores:
        // - The negative literal index.
        // - The positive literal index.
        // - The count of fresh unifications made on the disjunct.
        // When a recursive call fails, the literal indicies are restored and the unifications removed.
        // The positive literal index is also incremented by one, as it's the inner literal loop.
        let mut stack: Vec<(usize, usize, usize)> = Vec::default();

        let mut nl_index = 0;
        let mut pl_index = 0;

        let mut disjunct_index = 0;
        let limit = fs.len();

        while disjunct_index <= limit {
            if disjunct_index == limit {
                // Base case, there are no more sets to consider.
                return true;
            } else {
                // Work through every negative-positive pair

                match self.unify_complements_custom(
                    fs.set_at_index(disjunct_index),
                    nl_index,
                    pl_index,
                ) {
                    Ok((n_idx, p_idx, fresh_count)) => {
                        stack.push((n_idx, p_idx, fresh_count));
                        nl_index = 0;
                        pl_index = 0;
                        disjunct_index += 1;
                    }

                    Err(()) => match stack.pop() {
                        Some((n_idx, p_idx, fresh_count)) => {
                            disjunct_index -= 1;
                            pl_index = p_idx + 1;
                            nl_index = n_idx;
                            self.pop_multiple(fresh_count);
                        }

                        None => return false,
                    },
                }
            }
        }

        false
    }
}

impl std::fmt::Display for Unifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, (v, t)) in self.var_to_term.iter().enumerate() {
            write!(f, "{v}  =>  {t}")?;
            if idx + 1 < self.var_to_term.len() {
                write!(f, ",\t")?
            }
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

        let unification_result = u.unify_literals(&t1, &t2);
        assert_eq!(Ok(1), unification_result);
    }

    // #[test]
    fn udebug() {
        // let mut fm = FirstOrderFormula::from("exists x. (P(x) & ~P(x))");
        let mut fm = FirstOrderFormula::from("forall x. (P(x) | ~P(x))");
        fm = fm.generalize().negate().skolemize().simple_dnf();

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
