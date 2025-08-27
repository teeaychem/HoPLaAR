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
                            Imp => Formula::And(lhs.nnf_basic(), rhs.negate().nnf_basic()),
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

            _ => self,
        }
    }

    pub fn nnf(self) -> Self {
        self.simplify().nnf_basic()
    }

    fn nenf_basic(mut self) -> Self {
        use {Formula::*, OpBinary::*, OpUnary::*};

        match &mut self {
            Unary { op, expr } => match op {
                Not => match expr.as_mut() {
                    Unary { op, expr } => match op {
                        Not => std::mem::take(expr).nenf_basic(),
                    },

                    Binary { op, lhs, rhs } => {
                        let lhs = std::mem::take(lhs);
                        let rhs = std::mem::take(rhs);

                        match op {
                            And => Formula::Or(lhs.nenf_basic(), rhs.nenf_basic()),
                            Or => Formula::And(lhs.nenf_basic(), rhs.nenf_basic()),
                            Imp => Formula::And(lhs.nenf_basic(), rhs.negate().nenf_basic()),
                            Iff => Formula::Iff(lhs.nenf_basic(), rhs.negate().nenf_basic()),
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
                    And => Formula::And(lhs.nenf_basic(), rhs.nenf_basic()),
                    Or => Formula::Or(lhs.nenf_basic(), rhs.nenf_basic()),
                    Imp => Formula::Or(lhs.negate().nenf_basic(), rhs.nenf_basic()),
                    Iff => Formula::Iff(lhs.nenf_basic(), rhs.nenf_basic()),
                }
            }

            Quantifier { .. } => todo!(),

            _ => self,
        }
    }

    pub fn nenf(self) -> Self {
        self.simplify().nenf_basic()
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, parse_propositional_formula};

    #[test]
    fn nnf_simple() {
        let expr = parse_propositional_formula("(p <=> q) <=> ~(r ==> s)");
        let expr_nnf = expr.clone().nnf();

        assert!(Formula::Iff(expr, expr_nnf).tautology())
    }

    #[test]
    fn nenf_simple() {
        let expr = parse_propositional_formula("(p <=> q) <=> ~(r ==> s)");
        let expr_nenf = expr.clone().nenf();

        assert!(Formula::Iff(expr, expr_nenf).tautology())
    }
}
