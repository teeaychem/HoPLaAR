use std::collections::HashSet;

use itertools::Itertools;

use crate::logic::{
    Atomic,
    first_order::{FirstOrderFormula, Term, terms::Fun},
};

impl FirstOrderFormula {
    pub fn herbrand_split(&self) -> (HashSet<Fun>, HashSet<Fun>) {
        let mut functions: HashSet<Fun> = HashSet::default();
        let mut constants: HashSet<Fun> = HashSet::default();

        for function in self.functions() {
            match function.arity() {
                0 => constants.insert(function.clone()),
                _ => functions.insert(function.clone()),
            };
        }

        if constants.is_empty() {
            constants.insert(Fun {
                id: "c".to_owned(),
                variant: 0,
                args: Vec::default(),
            });
        }

        (constants, functions)
    }
}

struct Ground {
    constants: HashSet<Fun>,
    functions: HashSet<Fun>,
    ground: Vec<Fun>,
    separators: Vec<usize>,
}

impl Default for Ground {
    fn default() -> Self {
        Self {
            constants: Default::default(),
            functions: Default::default(),
            ground: Default::default(),
            separators: vec![0],
        }
    }
}

impl From<FirstOrderFormula> for Ground {
    fn from(value: FirstOrderFormula) -> Self {
        let mut ground = Ground::default();

        for relation in value.atoms_dfs() {
            for term in relation.parts() {
                match term {
                    Term::F(fun) => match fun.arity() {
                        0 => {
                            ground.constants.insert(fun.clone());
                        }
                        _ => {
                            ground.functions.insert(fun.clone());
                        }
                    },
                    Term::V(_) => {}
                }
            }
        }

        for constant in &ground.constants {
            ground.ground.push(constant.clone());
        }

        ground
    }
}

impl std::fmt::Display for Ground {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut separators = self.separators.iter();

        let mut start = separators.next().cloned().unwrap();
        let mut end = separators.next().cloned();
        while let Some(idx) = end {
            for ground_term in &self.ground[start..idx] {
                ground_term.fmt_ansi(f, true)?;
                write!(f, " ")?;
            }
            writeln!(f)?;

            start = idx;
            end = separators.next().cloned();
        }

        for ground_term in &self.ground[start..] {
            ground_term.fmt_ansi(f, true)?;
            write!(f, " ")?;
        }
        writeln!(f)?;

        Ok(())
    }
}

impl Ground {
    fn fresh_separator(&mut self) {
        self.separators.push(self.ground.len());
    }
}

impl Ground {
    pub fn size(&self) -> usize {
        self.ground.len()
    }

    // The initial ground atoms, or those from the most recent call to `overlay`.
    pub fn top_soil(&self) -> &[Fun] {
        match self.separators.last().cloned() {
            Some(s) => &self.ground[s..],
            None => &self.ground,
        }
    }
}

impl Ground {
    // Apples each function in `self` to the top soil.
    pub fn overlay(&mut self) {
        let mut overlay = Vec::default();

        for function in &self.functions {
            for args in self.top_soil().iter().permutations(function.arity()) {
                let fresh_f = Fun {
                    id: function.id.to_owned(),
                    variant: function.variant,
                    args: args.into_iter().map(|f| Term::F(f.to_owned())).collect(),
                };
                overlay.push(fresh_f);
            }
        }

        self.fresh_separator();
        self.ground.extend(overlay);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::first_order::FirstOrderFormula;

    #[test]
    fn ground_basic() {
        let fm = FirstOrderFormula::from("R(f(c())) | ~R(g(d()))");

        let mut ground = Ground::from(fm);
        println!("{ground}");
        assert_eq!(ground.size(), 2);

        ground.overlay();
        println!("{ground}");
        // 1 * 2 + 2 * 2
        assert_eq!(ground.size(), 6);

        ground.overlay();
        println!("{ground}");
        // 1 * 2 + 2 * 2 + 4 * 2
        assert_eq!(ground.size(), 14);
    }
}
