use crate::logic::{Atomic, Formula, OpBinary, OpUnary, Quantifier};

impl<A: Atomic> Formula<A> {
    pub fn dual(&self) -> Formula<A> {
        match self {
            Formula::True => Formula::False,
            Formula::False => Formula::True,
            Formula::Atom { .. } => self.to_owned(),
            Formula::Unary { op, expr } => match op {
                OpUnary::Not => Formula::Not(expr.dual()),
            },
            Formula::Binary { op, lhs, rhs } => match op {
                OpBinary::And => Formula::Or(lhs.dual(), rhs.dual()),
                OpBinary::Or => Formula::And(lhs.dual(), rhs.dual()),
                OpBinary::Imp => panic!("Dual of Imp"),
                OpBinary::Iff => panic!("Dual of Iff"),
            },
            Formula::Quantifier { q, var, expr } => match q {
                Quantifier::ForAll => Formula::Exists(var.to_owned(), expr.dual()),
                Quantifier::Exists => Formula::ForAll(var.to_owned(), expr.dual()),
            },
        }
    }

    pub fn simplify1(self) -> Self {
        use {Formula::*, OpBinary::*, OpUnary::*};

        match &self {
            Unary { op, expr } => match op {
                Not => match expr.as_ref() {
                    True => False,
                    False => True,

                    Unary { op, expr } => match op {
                        Not => *expr.clone(),
                    },
                    _ => self,
                },
            },

            Binary { op, lhs, rhs } => match op {
                And => match (lhs.as_ref(), rhs.as_ref()) {
                    (False, _) | (_, False) => False,
                    (True, expr) | (expr, True) => expr.clone(),
                    _ => self,
                },

                Or => match (lhs.as_ref(), rhs.as_ref()) {
                    (True, _) | (_, True) => True,
                    (False, expr) | (expr, False) => expr.clone(),
                    _ => self,
                },

                Imp => match (lhs.as_ref(), rhs.as_ref()) {
                    (False, _) | (_, True) => True,
                    (True, expr) => expr.clone(),
                    (expr, False) => Formula::Not(expr.clone()),

                    _ => self,
                },

                Iff => match (lhs.as_ref(), rhs.as_ref()) {
                    (expr, True) | (True, expr) => expr.clone(),
                    (expr, False) | (False, expr) => Formula::Not(expr.clone()),
                    _ => self,
                },
            },

            Quantifier { .. } => todo!(),

            _ => self,
        }
    }

    pub fn simplify(self) -> Self {
        match self {
            Formula::Unary { op, expr } => Formula::Unary(op, expr.simplify()).simplify1(),
            Formula::Binary { op, lhs, rhs } => {
                Formula::Binary(op, lhs.simplify(), rhs.simplify()).simplify1()
            }

            Formula::Quantifier { .. } => todo!(),

            _ => self,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{parsing::parse_propositional_formula, propositional::Valuation};

    #[test]
    fn duals() {
        let expr = parse_propositional_formula("p | ~ p");
        let expected = parse_propositional_formula("p & ~ p");
        assert_eq!(expr.dual(), expected);
    }

    #[test]
    fn thm_2_7() {
        let expr = parse_propositional_formula("p & q");
        let v_a = Valuation::from_prop_set(expr.atoms());
        assert_eq!(expr.eval(&v_a), !expr.dual().eval(&v_a.inverted()))
    }

    #[test]
    fn simplification() {
        let expr = parse_propositional_formula("(true ==> (x <=> false)) ==> ~(y | false & z)");
        let expected = parse_propositional_formula("~x ==> ~y");

        assert_eq!(expr.simplify(), expected);

        let expr = parse_propositional_formula("((x ==> y) ==> true) | ~false");
        let expected = parse_propositional_formula("true");

        assert_eq!(expr.simplify(), expected);
    }
}
