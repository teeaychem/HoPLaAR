use std::collections::HashMap;

use crate::logic::first_order::{
    Term,
    terms::{Fun, Var},
};

pub type Eqs = Vec<(Term, Term)>;

#[derive(Clone, Debug, Default)]
pub struct Unifier {
    env: HashMap<Var, Term>,
}

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
            Term::V(y) => match self.env.get(y) {
                Some(t_y) => self.get_map_type(x, t_y),
                None => MapType::Fresh,
            },
        }
    }
}

impl Unifier {
    ///
    ///
    /// An iterative variant of the books recursive implementation.
    ///
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
                    if let Some(y) = self.env.get(&x) {
                        eqs.push((y.clone(), t));
                    } else {
                        match self.get_map_type(&x, &t) {
                            MapType::Trivial => {}
                            MapType::Fresh => {
                                self.env.insert(x, t);
                            }
                            MapType::Cyclic => panic!("Cyclic"),
                        }
                    }
                }
            }
        }
    }

    pub fn solve_step(&mut self) {}
}

impl std::fmt::Display for Unifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, v) in &self.env {
            writeln!(f, "{k} -> {v}")?
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
        let t1 = Term::try_from("f(y)").unwrap();
        let t2 = Term::try_from("x").unwrap();
        let t3 = Term::try_from("g(x)").unwrap();
        let t4 = Term::try_from("y").unwrap();

        u.unify(vec![(t1, t2), (t3, t4)]);
        println!("{u}");
    }
}
