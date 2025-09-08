use std::collections::HashSet;

use itertools::{Itertools, repeat_n};

use crate::logic::{
    Atomic,
    first_order::{
        FirstOrderFormula, Term,
        syntax::Substitution,
        terms::{Fun, Var},
    },
    propositional::PropFormula,
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

pub struct Ground {
    formula: FirstOrderFormula,
    free_variables: Vec<Var>,
    functions: HashSet<Fun>,
    ground: Vec<Fun>,
    level_markers: Vec<usize>,
}

impl From<&FirstOrderFormula> for Ground {
    fn from(value: &FirstOrderFormula) -> Self {
        let mut constants: HashSet<&Fun> = HashSet::default();
        let mut functions = HashSet::default();

        for relation in value.atoms_dfs() {
            for term in relation.parts() {
                match term {
                    Term::F(fun) => match fun.arity() {
                        0 => {
                            constants.insert(fun);
                        }
                        _ => {
                            functions.insert(fun.clone());
                        }
                    },
                    Term::V(_) => {}
                }
            }
        }

        let mut ground: Vec<Fun> = constants.into_iter().cloned().collect();
        if ground.is_empty() {
            let c = Fun {
                id: "c".to_owned(),
                variant: 0,
                args: Vec::default(),
            };
            ground.push(c);
        }

        Ground {
            free_variables: value.free_variables().iter().cloned().collect(),
            formula: value.to_owned(),
            functions,
            ground,
            level_markers: vec![0],
        }
    }
}

impl std::fmt::Display for Ground {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut separators = self.level_markers.iter();

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
        self.level_markers.push(self.ground.len());
    }
}

impl Ground {
    pub fn size(&self) -> usize {
        self.ground.len()
    }

    // The initial ground atoms, or those from the most recent call to `overlay`.
    pub fn top_soil(&self) -> &[Fun] {
        match self.level_markers.last().cloned() {
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

    pub fn top_soil_formulas(&self) -> impl Iterator<Item = PropFormula> {
        repeat_n(self.top_soil().iter(), self.formula.free_variables().len())
            .multi_cartesian_product()
            .map(|ground_permutation| {
                let mut s = Substitution::default();
                for (k, &v) in self.free_variables.iter().zip(&ground_permutation) {
                    s.add_interrupt(k, Some(v.into()));
                }
                PropFormula::try_from(self.formula.clone().term_substitution(&mut s)).unwrap()
            })
    }
}

impl FirstOrderFormula {
    pub fn is_valid_gilmore(&self, limit: Option<usize>) -> (bool, Ground, PropFormula) {
        let sfm = self.clone().generalize().negate().skolemize();
        let mut ground = Ground::from(&sfm);
        let mut propositional = PropFormula::conjoin(ground.top_soil_formulas());

        for _ in 0..limit.unwrap_or(usize::MAX) {
            if propositional.is_unsatisfiable() {
                return (true, ground, propositional);
            }
            ground.overlay();
            propositional = PropFormula::And(
                propositional,
                PropFormula::conjoin(ground.top_soil_formulas()),
            );
        }

        (false, ground, propositional)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::logic::first_order::FirstOrderFormula;

    #[test]
    fn ground_basic() {
        let fm = FirstOrderFormula::from("R(f(c())) | ~R(g(d()))");

        let mut ground = Ground::from(&fm);
        assert_eq!(ground.size(), 2);

        ground.overlay();
        // 1 * 2 + 2 * 2
        assert_eq!(ground.size(), 6);

        ground.overlay();
        // 1 * 2 + 2 * 2 + 4 * 2
        assert_eq!(ground.size(), 14);
    }

    #[test]
    fn ground_instance() {
        let fm = FirstOrderFormula::from("~R(x) | R(f(g(y)))");

        let mut ground = Ground::from(&fm);

        for _ in 0..2 {
            for tsf in ground.top_soil_formulas() {
                println!("{tsf}");
            }
            ground.overlay();
        }
    }

    #[test]
    fn gilmore_basic() {
        let fm = FirstOrderFormula::from("exists x. forall y. (P(x) => P(y))");
        let (result, _ground, _propositional) = fm.is_valid_gilmore(Some(3));
        assert!(result);
    }
}
