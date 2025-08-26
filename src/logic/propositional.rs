use std::collections::{HashMap, HashSet};

use crate::logic::{Atomic, Formula};

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq)]
pub struct Prop {
    name: String,
}

impl Atomic for Prop {}

impl Prop {
    pub fn from(name: &str) -> Self {
        Self {
            name: name.to_owned(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn name_set(&mut self, name: String) {
        self.name = name
    }
}

// Propositional formula
pub type PropFormula = Formula<Prop>;

#[derive(Default, Debug)]
pub struct Valuation {
    values: Vec<bool>,
    map: HashMap<Prop, usize>,
}

impl Valuation {
    pub fn from_props<const N: usize>(props: [Prop; N]) -> Self {
        let mut valuation = Valuation::default();
        for prop in props {
            valuation.extend(prop, false);
        }
        valuation
    }

    pub fn from_names<const N: usize>(names: [&str; N]) -> Self {
        let mut valuation = Valuation::default();
        for name in names {
            valuation.extend(Prop::from(name), false);
        }
        valuation
    }

    pub fn from_prop_set(props: HashSet<Prop>) -> Self {
        let mut valuation = Valuation::default();
        for prop in props {
            valuation.extend(prop, false);
        }
        valuation
    }
}

impl Valuation {
    pub fn extend(&mut self, prop: Prop, val: bool) {
        self.map.insert(prop, self.values.len());
        self.values.push(val);
    }

    pub fn get(&self, prop: &Prop) -> bool {
        self.values[self.map[prop]]
    }

    // Views the valuation as a base two int and increments by one.
    //
    // Strictly, as a reversed base two int, as the bits are bumped from the left.
    pub fn next_permutation_mut(&mut self) {
        for val in self.values.iter_mut() {
            match val {
                true => *val = false,
                false => {
                    *val = true;
                    break;
                }
            }
        }
    }

    pub fn size(&self) -> usize {
        self.values.len()
    }

    pub fn permutation_count(&self) -> usize {
        2_usize.pow(
            self.size()
                .try_into()
                .expect("Permutation count exceeds usize"),
        )
    }

    pub fn atoms(&self) -> impl Iterator<Item = &Prop> {
        self.map.keys()
    }

    pub fn map(&self) -> &HashMap<Prop, usize> {
        &self.map
    }
}

pub fn eval(formula: &PropFormula, valuation: &Valuation) -> bool {
    match formula {
        Formula::True => true,

        Formula::False => false,

        Formula::Atom { var } => valuation.get(var),

        Formula::OpUnary { op, expr } => match op {
            crate::logic::OpUnary::Not => !eval(expr, valuation),
        },

        Formula::OpBinary { op, lhs, rhs } => match op {
            crate::logic::OpBinary::And => lhs.eval(valuation) && rhs.eval(valuation),
            crate::logic::OpBinary::Or => lhs.eval(valuation) || rhs.eval(valuation),
            crate::logic::OpBinary::Imp => !lhs.eval(valuation) || rhs.eval(valuation),
            crate::logic::OpBinary::Iff => lhs.eval(valuation) == rhs.eval(valuation),
        },

        Formula::Quantifier { .. } => todo!(),
    }
}

impl PropFormula {
    pub fn eval(&self, valuation: &Valuation) -> bool {
        eval(self, valuation)
    }

    pub fn on_all_valuations<F: Fn(&PropFormula, &Valuation) -> bool>(&self, f: &F) -> bool {
        let mut valuation = Valuation::from_prop_set(self.atoms());
        for _ in 0..valuation.permutation_count() {
            match f(self, &valuation) {
                true => {}
                false => return false,
            }
            valuation.next_permutation_mut();
        }
        true
    }

    pub fn print_truth_table(&self) {
        let mut valuation = Valuation::from_prop_set(self.atoms());

        let spacing = 2 + valuation
            .atoms()
            .fold(7, |a, n| std::cmp::max(a, n.name().len()));
        let total_width = spacing * (valuation.size() + 1);

        let mut atoms = valuation.map().iter().collect::<Vec<_>>();
        atoms.sort_by(|(_, a), (_, b)| a.cmp(b));

        println!("{self}");

        for (atom, _) in atoms {
            print!("{name:width$}", width = spacing, name = atom.name());
        }

        println!("| {eval:width$}", width = spacing, eval = "formula");
        println!("{:-<total_width$}", "");

        for _ in 0..valuation.permutation_count() {
            for value in &valuation.values {
                print!("{value:width$}", width = spacing);
            }
            println!(
                "| {eval:width$}",
                width = spacing,
                eval = self.eval(&valuation)
            );
            valuation.next_permutation_mut();
        }
        println!("{:-<total_width$}", "");
    }

    pub fn tautology(&self) -> bool {
        let mut valuation = Valuation::from_prop_set(self.atoms());
        for _ in 0..valuation.permutation_count() {
            if !self.eval(&valuation) {
                return false;
            }
            valuation.next_permutation_mut();
        }
        true
    }

    pub fn unsatisfiable(&self) -> bool {
        let negated = PropFormula::Not(self.clone());
        negated.tautology()
    }

    pub fn satisfiable(&self) -> bool {
        let mut valuation = Valuation::from_prop_set(self.atoms());
        for _ in 0..valuation.permutation_count() {
            if self.eval(&valuation) {
                return true;
            }
            valuation.next_permutation_mut();
        }
        false
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::{
        parsing::parse_propositional_formula,
        propositional::{Prop, Valuation, eval},
    };

    #[test]
    fn eval_empty() {
        let v = Valuation::default();

        let expr = parse_propositional_formula("true or false");
        assert!(expr.eval(&v));

        let expr = parse_propositional_formula("true and false");
        assert!(!expr.eval(&v));
    }

    #[test]
    fn eval_small() {
        let mut v = Valuation::default();
        v.extend(Prop::from("a"), true);
        v.extend(Prop::from("b"), false);

        let expr = parse_propositional_formula("a or b");
        assert!(expr.eval(&v));

        let expr = parse_propositional_formula("a and b");
        assert!(!expr.eval(&v));

        let expr = parse_propositional_formula("a ==> b");
        assert!(!expr.eval(&v));

        let expr = parse_propositional_formula("~a ==> b");
        assert!(expr.eval(&v));

        let expr = parse_propositional_formula("~(a <=> b)");
        assert!(expr.eval(&v));
    }

    #[test]
    fn all_valuations() {
        let expr = parse_propositional_formula("p and (q or r) iff (p and q) or (p and r)");
        assert!(expr.on_all_valuations(&eval));

        let expr = parse_propositional_formula("p and (q or r) iff (p or q) and (p or r)");
        assert!(!expr.on_all_valuations(&eval));
    }

    #[test]
    fn tautologies() {
        let a = parse_propositional_formula("p | ~p");
        assert!(a.tautology());

        let b = parse_propositional_formula("p | q ==> p");
        assert!(!b.tautology());

        let c = parse_propositional_formula("p | q => q | (p <=> q)");
        assert!(!c.tautology());

        let d = parse_propositional_formula("(p | q) & ~(p & q) ==> (~p <=> q)");
        assert!(d.tautology());
    }

    #[test]
    fn satisfiability() {
        let a = parse_propositional_formula("p | ~p");
        assert!(a.satisfiable());
        assert!(!a.unsatisfiable());

        let b = parse_propositional_formula("p | q ==> p");
        assert!(b.satisfiable());
        assert!(!b.unsatisfiable());

        let c = parse_propositional_formula("p & ~p");
        assert!(c.unsatisfiable())
    }
}
