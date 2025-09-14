use crate::logic::first_order::{
    Term,
    terms::{Fun, Var},
};

pub type Eqs = Vec<(Term, Term)>;

/// A mapping `from` a variable `to` a term.
#[derive(Clone, Debug)]
pub struct Mapping {
    from: Var,
    to: Term,
}

/// A struct which handles the state of unification, and bundles methods for unification.
#[derive(Clone, Debug, Default)]
pub struct Unifier {
    /// A unification is a collection of [Mapping]s.
    env: Vec<Mapping>,
}

/// The type of mapping, with respect to some background env.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MapType {
    Trivial,
    Fresh,
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
            Term::V(y) => {
                let v = self.env.binary_search_by(|m| m.from.cmp(y));
                match v {
                    Ok(index) => self.get_map_type(x, &self.env[index].to),
                    Err(_) => MapType::Fresh,
                }
            }
        }
    }

    /// Returns the index of variable `v` in the env.
    pub fn get_index(&self, v: &Var) -> Option<usize> {
        self.env.binary_search_by(|m| m.from.cmp(v)).ok()
    }

    /// Returns the term which variable `v` maps to in the unification environment.
    pub fn get_value(&self, v: &Var) -> Option<&Term> {
        match self.get_index(v) {
            Some(index) => Some(&self.env[index].to),
            None => None,
        }
    }
}

impl Unifier {
    /// Inserts a mapping from `v` to `t` into the unification environment.
    pub fn insert(&mut self, v: Var, t: Term) {
        self.env.push(Mapping { from: v, to: t });
        self.env.sort_by(|a, b| a.from.cmp(&b.from));
    }

    /// Unifies a sequences of equals.
    ///
    /// An iterative variant of a recursive implementation from the book.
    pub fn unify(&mut self, mut eqs: Eqs) {
        while let Some((lhs, rhs)) = eqs.pop() {
            match (lhs, rhs) {
                (Term::F(f), Term::F(g)) => {
                    if f == g {
                        eqs.extend(f.args.iter().cloned().zip(g.args.iter().cloned()));
                    } else {
                        panic!("Impossible")
                    }
                }

                (Term::V(x), t) | (t, Term::V(x)) => {
                    if let Some(y) = self.get_value(&x) {
                        eqs.push((y.clone(), t));
                    } else {
                        match self.get_map_type(&x, &t) {
                            MapType::Trivial => {}
                            MapType::Fresh => {
                                self.insert(x, t);
                            }
                            MapType::Cyclic => panic!("Cyclic"),
                        }
                    }
                }
            }
        }
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

        for index in 0..self.env.len() {
            let to = std::mem::take(&mut self.env[index].to);
            match self.update_term(to) {
                (t, true) => {
                    update = true;
                    self.env[index].to = t
                }
                (t, false) => self.env[index].to = t,
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
    pub fn fully_unify(&mut self, eqs: Eqs) {
        self.unify(eqs);
        self.solve();
    }
}

impl std::fmt::Display for Unifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for map in &self.env {
            writeln!(f, "{} -> {}", map.from, map.to)?
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::first_order::{Term, unification::Unifier};

    #[test]
    fn debug() {
        let mut u = Unifier::default();
        let t1 = Term::try_from("a").unwrap();
        let t2 = Term::try_from("b").unwrap();
        let t3 = Term::try_from("c").unwrap();
        let t4 = Term::try_from("d").unwrap();
        let t5 = Term::try_from("a").unwrap();
        let t6 = Term::try_from("c").unwrap();

        u.unify(vec![(t1, t2), (t3, t4), (t5, t6)]);

        u.solve();
        println!("{u}")
    }
}
