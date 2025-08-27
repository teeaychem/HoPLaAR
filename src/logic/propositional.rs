use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use crate::logic::{Atomic, Formula, OpBinary, OpUnary};

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

#[derive(Clone, Default, Debug)]
pub struct Valuation {
    assignment: Vec<(Prop, bool)>,
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
        self.map.insert(prop.clone(), self.assignment.len());
        self.assignment.push((prop.clone(), val));
    }

    pub fn get(&self, prop: &Prop) -> bool {
        self.assignment[self.map[prop]].1
    }

    pub fn set(&mut self, prop: &Prop, value: bool) {
        self.assignment[self.map[prop]].1 = value
    }

    // Views the valuation as a base two int and increments by one.
    //
    // Strictly, as a reversed base two int, as the bits are bumped from the left.
    pub fn next_permutation_mut(&mut self) {
        for (_, val) in self.assignment.iter_mut() {
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
        self.assignment.len()
    }

    pub fn permutation_count(&self) -> usize {
        2_usize.pow(
            self.size()
                .try_into()
                .expect("Permutation count exceeds usize"),
        )
    }

    pub fn assignment(&self) -> &[(Prop, bool)] {
        &self.assignment
    }

    pub fn map(&self) -> &HashMap<Prop, usize> {
        &self.map
    }

    pub fn invert(&mut self) {
        for (_, value) in self.assignment.iter_mut() {
            *value = !*value;
        }
    }

    pub fn inverted(&self) -> Valuation {
        let mut inverted = self.clone();
        inverted.invert();
        inverted
    }

    pub fn as_formula(&self) -> PropFormula {
        // In reverse so the constructed formula matches an equivalent parse without parens.
        let mut assignment = self.assignment.iter().rev();

        // An initial match to avoid top as the first conjunct.
        match assignment.next() {
            None => PropFormula::True,

            Some((prop, val)) => {
                let atom = PropFormula::Atom(prop.clone());

                let literal = match val {
                    true => atom,
                    false => PropFormula::Not(atom),
                };

                let mut formula = literal;

                #[allow(clippy::while_let_on_iterator)]
                while let Some((prop, val)) = assignment.next() {
                    let atom = PropFormula::Atom(prop.clone());
                    let literal = match val {
                        true => atom,
                        false => PropFormula::Not(atom),
                    };

                    formula = PropFormula::And(literal, formula)
                }

                formula
            }
        }
    }
}


pub fn eval(formula: &PropFormula, valuation: &Valuation) -> bool {
    match formula {
        Formula::True => true,

        Formula::False => false,

        Formula::Atom { var } => valuation.get(var),

        Formula::Unary { op, expr } => match op {
            OpUnary::Not => !eval(expr, valuation),
        },

        Formula::Binary { op, lhs, rhs } => match op {
            OpBinary::And => lhs.eval(valuation) && rhs.eval(valuation),
            OpBinary::Or => lhs.eval(valuation) || rhs.eval(valuation),
            OpBinary::Imp => !lhs.eval(valuation) || rhs.eval(valuation),
            OpBinary::Iff => lhs.eval(valuation) == rhs.eval(valuation),
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

    pub fn truth_table(&self) -> String {
        let mut table = String::default();

        let mut valuation = Valuation::from_prop_set(self.atoms());

        let spacing = 1 + valuation
            .assignment()
            .iter()
            .fold(7, |a, (n, _)| std::cmp::max(a, n.name().len()));
        let total_width = spacing * (valuation.size() + 1);

        for (prop, _) in valuation.assignment() {
            let _ = write!(table, "{name:width$}", width = spacing, name = prop.name());
        }

        let _ = writeln!(table, "| {eval:width$}", width = spacing, eval = "formula");
        let _ = writeln!(table, "{:-<total_width$}", "");

        for _ in 0..valuation.permutation_count() {
            for (_, value) in &valuation.assignment {
                let _ = write!(table, "{value:width$}", width = spacing);
            }
            let _ = writeln!(
                table,
                "| {eval:width$}",
                width = spacing,
                eval = self.eval(&valuation)
            );
            valuation.next_permutation_mut();
        }
        let _ = writeln!(table, "{:-<total_width$}", "");

        table
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

    pub fn dnf(&self) -> PropFormula {
        let mut formula = PropFormula::False;

        let mut valuation = Valuation::from_prop_set(self.atoms());
        for _ in 0..valuation.permutation_count() {
            if self.eval(&valuation) {
                match formula {
                    PropFormula::False => formula = valuation.as_formula(),
                    _ => formula = PropFormula::Or(formula, valuation.as_formula()),
                };
            }
            valuation.next_permutation_mut();
        }

        formula
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::{
        parsing::parse_propositional_formula,
        propositional::{Prop, PropFormula, Valuation, eval},
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

    #[test]
    fn as_formula() {
        let mut valuation = Valuation::from_names(["a", "b", "c", "d"]);

        let expr = parse_propositional_formula("~a & ~b & ~c & ~d");

        assert_eq!(valuation.as_formula(), expr);

        valuation.set(&Prop::from("b"), true);
        let expr = parse_propositional_formula("~a & b & ~c & ~d");

        assert_eq!(valuation.as_formula(), expr);

        valuation.invert();
        let expr = parse_propositional_formula("a & ~b & c & d");

        assert_eq!(valuation.as_formula(), expr);
    }

    #[test]
    fn cnf() {
        let expr = parse_propositional_formula("(p | q & r) & (~p | ~r)");

        assert!(PropFormula::Iff(expr.clone(), expr.dnf()).tautology());
    }
}
