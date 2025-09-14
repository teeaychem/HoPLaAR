use crate::logic::first_order::{
    Term,
    terms::{Fun, Var},
};

pub type Eqs = Vec<(Term, Term)>;

#[derive(Clone, Debug)]
pub struct Mapping {
    from: Var,
    to: Term,
}

#[derive(Clone, Debug, Default)]
pub struct Unifier {
    env: Vec<Mapping>,
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
            Term::V(y) => {
                let v = self.env.binary_search_by(|m| m.from.cmp(y));
                match v {
                    Ok(index) => self.get_map_type(x, &self.env[index].to),
                    Err(_) => MapType::Fresh,
                }
            }
        }
    }

    pub fn get_index(&self, k: &Var) -> Option<usize> {
        self.env.binary_search_by(|m| m.from.cmp(k)).ok()
    }

    pub fn get_value(&self, k: &Var) -> Option<&Term> {
        match self.get_index(k) {
            Some(index) => Some(&self.env[index].to),
            None => None,
        }
    }
}

impl Unifier {
    pub fn insert(&mut self, v: Var, t: Term) {
        self.env.push(Mapping { from: v, to: t });
        self.env.sort_by(|a, b| a.from.cmp(&b.from));
    }

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

    pub fn update_term(&mut self, t: Term) -> Term {
        match t {
            Term::F(Fun { id, variant, args }) => Term::F(Fun {
                id,
                variant,
                args: args.into_iter().map(|arg| self.update_term(arg)).collect(),
            }),
            Term::V(ref var) => match self.get_value(var) {
                Some(y) => y.clone(),
                None => t,
            },
        }
    }

    pub fn solve_one_pass(&mut self) {
        for index in 0..self.env.len() {
            let to = std::mem::take(&mut self.env[index].to);
            self.env[index].to = self.update_term(to)
        }
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
        let t3 = Term::try_from("f(a)").unwrap();
        let t4 = Term::try_from("c").unwrap();

        u.unify(vec![(t1, t2), (t3, t4)]);

        println!("{u}");
        u.solve_one_pass();

        println!("{u}");
    }
}
