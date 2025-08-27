use crate::logic::{Atomic, Formula, OpBinary, OpUnary};

impl<A: Atomic> Formula<A> {
    fn nnf_inner(self) -> Self {
        use {Formula::*, OpBinary::*, OpUnary::*};

        match self {
            Unary { op, ref expr } => match op {
                Not => match expr.as_ref() {
                    Unary { op, expr } => match op {
                        Not => expr.nnf(),
                    },

                    Binary { op, lhs, rhs } => match op {
                        And => Formula::Or(lhs.nnf(), rhs.nnf()),
                        Or => Formula::And(lhs.nnf(), rhs.nnf()),
                        Imp => Formula::And(lhs.nnf(), rhs.clone().negate().nnf()),
                        Iff => {
                            let a = Formula::And(lhs.nnf(), rhs.clone().negate().nnf());
                            let b = Formula::And(rhs.nnf(), lhs.clone().negate().nnf());
                            Formula::Or(a, b)
                        }
                    },

                    Quantifier { .. } => todo!(),

                    _ => self,
                },
            },
            Binary { op, lhs, rhs } => match op {
                And => Formula::And(lhs.nnf(), rhs.nnf()),
                Or => Formula::Or(lhs.nnf(), rhs.nnf()),
                Imp => Formula::Or(lhs.negate().nnf(), rhs.nnf()),
                Iff => {
                    let a = Formula::And(lhs.nnf(), rhs.nnf());
                    let b = Formula::And(lhs.negate().nnf(), rhs.negate().nnf());
                    Formula::Or(a, b)
                }
            },

            Quantifier { .. } => todo!(),

            _ => self.clone(),
        }
    }

    pub fn nnf(&self) -> Self {
        self.clone().simplify().nnf_inner()
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, parse_propositional_formula};

    #[test]
    fn simple() {
        let expr = parse_propositional_formula("(p <=> q) <=> ~(r ==> s)");
        let expr_nnf = expr.nnf();

        assert!(Formula::Iff(expr, expr_nnf).tautology())
    }
}
