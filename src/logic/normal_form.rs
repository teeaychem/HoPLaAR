use crate::logic::{Atomic, Formula, OpBinary, OpUnary};

impl<A: Atomic> Formula<A> {
    fn nnf_basic(mut self) -> Self {
        use {Formula::*, OpBinary::*, OpUnary::*};

        match &mut self {
            Unary { op, expr } => match op {
                Not => match expr.as_mut() {
                    Unary { op, expr } => match op {
                        Not => std::mem::take(expr).nnf_basic(),
                    },

                    Binary { op, lhs, rhs } => {
                        let lhs = std::mem::take(lhs);
                        let rhs = std::mem::take(rhs);

                        match op {
                            And => Formula::Or(lhs.nnf_basic(), rhs.nnf_basic()),
                            Or => Formula::And(lhs.nnf_basic(), rhs.nnf_basic()),
                            Imp => Formula::And(lhs.nnf_basic(), rhs.clone().negate().nnf_basic()),
                            Iff => {
                                let a = Formula::And(
                                    lhs.clone().nnf_basic(),
                                    rhs.clone().negate().nnf_basic(),
                                );
                                let b = Formula::And(lhs.negate().nnf_basic(), rhs.nnf_basic());
                                Formula::Or(a, b)
                            }
                        }
                    }

                    Quantifier { .. } => todo!(),

                    _ => self,
                },
            },
            Binary { op, lhs, rhs } => {
                let lhs = std::mem::take(lhs);
                let rhs = std::mem::take(rhs);
                match op {
                    And => Formula::And(lhs.nnf_basic(), rhs.nnf_basic()),
                    Or => Formula::Or(lhs.nnf_basic(), rhs.nnf_basic()),
                    Imp => Formula::Or(lhs.negate().nnf_basic(), rhs.nnf_basic()),
                    Iff => {
                        let a = Formula::And(lhs.clone().nnf_basic(), rhs.clone().nnf_basic());
                        let b = Formula::And(lhs.negate().nnf_basic(), rhs.negate().nnf_basic());
                        Formula::Or(a, b)
                    }
                }
            }

            Quantifier { .. } => todo!(),

            _ => self.clone(),
        }
    }

    pub fn nnf(self) -> Self {
        self.clone().simplify().nnf_basic()
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, parse_propositional_formula};

    #[test]
    fn simple() {
        let expr = parse_propositional_formula("(p <=> q) <=> ~(r ==> s)");
        let expr_nnf = expr.clone().nnf();

        assert!(Formula::Iff(expr, expr_nnf).tautology())
    }
}
