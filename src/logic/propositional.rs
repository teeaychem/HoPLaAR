use std::collections::{HashMap, HashSet};

use crate::logic::{Atomic, Formula};

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq)]
pub struct Prop {
    name: String,
}

impl Atomic for Prop {}

impl Prop {
    pub fn new(name: &str) -> Self {
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
            valuation.extend(Prop::new(name), false);
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
        v.extend(Prop::new("a"), true);
        v.extend(Prop::new("b"), false);

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
}
