use std::fmt::Write;

use crate::logic::{Formula, OpBinary, OpUnary, first_order::FirstOrderFormula};

mod bdd;
mod cnf;

mod prop_dict;
pub use prop_dict::PropDict;

mod prop;
pub use prop::Prop;

mod valuation;
pub use valuation::Valuation;

pub use crate::logic::parse::parse_propositional as parse;

// Propositional formula
pub type PropFormula = Formula<Prop>;

pub fn eval(formula: &PropFormula, valuation: &Valuation) -> bool {
    match formula {
        Formula::True => true,

        Formula::False => false,

        Formula::Atom(atom) => valuation.get(atom),

        Formula::Unary { op, fml: expr } => match op {
            OpUnary::Not => !eval(expr, valuation),
        },

        Formula::Binary { op, lhs, rhs } => match op {
            OpBinary::And => lhs.eval(valuation) && rhs.eval(valuation),
            OpBinary::Or => lhs.eval(valuation) || rhs.eval(valuation),
            OpBinary::Imp => !lhs.eval(valuation) || rhs.eval(valuation),
            OpBinary::Iff => lhs.eval(valuation) == rhs.eval(valuation),
        },

        Formula::Quantified { .. } => todo!(),
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
            .fold(7, |a, literal| std::cmp::max(a, literal.id().len()));
        let total_width = spacing * (valuation.size() + 1);

        for literal in valuation.assignment() {
            let _ = write!(table, "{id:width$}", width = spacing, id = literal.id());
        }

        let _ = writeln!(table, "| {eval:width$}", width = spacing, eval = "formula");
        let _ = writeln!(table, "{:-<total_width$}", "");

        for _ in 0..valuation.permutation_count() {
            for literal in valuation.assignment() {
                let _ = write!(table, "{:width$}", literal.value, width = spacing);
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

    pub fn is_tautology(&self) -> bool {
        let mut valuation = Valuation::from_prop_set(self.atoms());
        let permutation_count = valuation.permutation_count();

        for _ in 0..permutation_count {
            if !self.eval(&valuation) {
                return false;
            }
            valuation.next_permutation_mut();
        }
        true
    }

    pub fn is_unsatisfiable(&self) -> bool {
        let negated = self.clone().negate();
        negated.is_tautology()
    }

    pub fn is_satisfiable(&self) -> bool {
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

impl TryFrom<FirstOrderFormula> for PropFormula {
    type Error = ();

    fn try_from(value: FirstOrderFormula) -> Result<PropFormula, Self::Error> {
        match value {
            FirstOrderFormula::True => Ok(PropFormula::True),
            FirstOrderFormula::False => Ok(PropFormula::False),

            FirstOrderFormula::Atom(relation) => Ok(PropFormula::Atom(Prop::from(relation))),

            FirstOrderFormula::Unary { op, fml: expr } => {
                Ok(PropFormula::Unary(op, PropFormula::try_from(*expr)?))
            }

            FirstOrderFormula::Binary { op, lhs, rhs } => Ok(PropFormula::Binary(
                op,
                PropFormula::try_from(*lhs)?,
                PropFormula::try_from(*rhs)?,
            )),

            FirstOrderFormula::Quantified { .. } => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::{
        first_order::FirstOrderFormula,
        propositional::{Prop, PropFormula, Valuation, eval, parse},
    };

    #[test]
    fn eval_empty() {
        let v = Valuation::default();

        let expr = parse("true or false");
        assert!(expr.eval(&v));

        let expr = parse("true and false");
        assert!(!expr.eval(&v));
    }

    #[test]
    fn eval_small() {
        let mut v = Valuation::default();
        v.extend(Prop::from("a"), true);
        v.extend(Prop::from("b"), false);

        let expr = parse("a or b");
        assert!(expr.eval(&v));

        let expr = parse("a and b");
        assert!(!expr.eval(&v));

        let expr = parse("a ==> b");
        assert!(!expr.eval(&v));

        let expr = parse("~a ==> b");
        assert!(expr.eval(&v));

        let expr = parse("~(a <=> b)");
        assert!(expr.eval(&v));
    }

    #[test]
    fn all_valuations() {
        let expr = parse("p and (q or r) iff (p and q) or (p and r)");
        assert!(expr.on_all_valuations(&eval));

        let expr = parse("p and (q or r) iff (p or q) and (p or r)");
        assert!(!expr.on_all_valuations(&eval));
    }

    #[test]
    fn tautologies() {
        let a = parse("p | ~p");
        assert!(a.is_tautology());

        let b = parse("p | q ==> p");
        assert!(!b.is_tautology());

        let c = parse("p | q => q | (p <=> q)");
        assert!(!c.is_tautology());

        let d = parse("(p | q) & ~(p & q) ==> (~p <=> q)");
        assert!(d.is_tautology());
    }

    #[test]
    fn satisfiability() {
        let a = parse("p | ~p");
        assert!(a.is_satisfiable());
        assert!(!a.is_unsatisfiable());

        let b = parse("p | q ==> p");
        assert!(b.is_satisfiable());
        assert!(!b.is_unsatisfiable());

        let c = parse("p & ~p");
        assert!(c.is_unsatisfiable())
    }

    #[test]
    fn as_formula() {
        let mut valuation = Valuation::from_ids(["a", "b", "c", "d"]);

        let expr = parse("~a & ~b & ~c & ~d");

        assert_eq!(valuation.as_formula(), expr);

        valuation.set(&Prop::from("b"), true);
        let expr = parse("~a & b & ~c & ~d");

        assert_eq!(valuation.as_formula(), expr);

        valuation.invert();
        let expr = parse("a & ~b & c & d");

        assert_eq!(valuation.as_formula(), expr);
    }

    #[test]
    fn cnf() {
        let expr = parse("(p | q & r) & (~p | ~r)");

        assert!(PropFormula::Iff(expr.clone(), expr.dnf()).is_tautology());
    }

    #[test]
    fn try_from_first_order() {
        let basic = FirstOrderFormula::from("P(x) | ~P(x)");
        let attempt = PropFormula::try_from(basic);
        assert!(attempt.is_ok_and(|fm| fm.is_tautology()));

        let basic = FirstOrderFormula::from("forall x. (P(x) | ~P(x))");
        let attempt = PropFormula::try_from(basic);
        assert!(attempt.is_err())
    }
}
