use crate::logic::{Atomic, Formula, OpBinary, OpUnary};

impl<A: Atomic> Formula<A> {
    pub fn negation_normal_form_basic(mut self) -> Self {
        use {Formula::*, OpBinary::*, OpUnary::*};

        match &mut self {
            Unary { op, expr } => match op {
                Not => match expr.as_mut() {
                    Unary { op, expr } => match op {
                        Not => std::mem::take(expr).negation_normal_form_basic(),
                    },

                    Binary { op, lhs, rhs } => {
                        let lhs = std::mem::take(lhs);
                        let rhs = std::mem::take(rhs);

                        match op {
                            And => Formula::Or(
                                lhs.negate().negation_normal_form_basic(),
                                rhs.negate().negation_normal_form_basic(),
                            ),
                            Or => Formula::And(
                                lhs.negate().negation_normal_form_basic(),
                                rhs.negate().negation_normal_form_basic(),
                            ),
                            Imp => Formula::And(
                                lhs.negation_normal_form_basic(),
                                rhs.negate().negation_normal_form_basic(),
                            ),
                            Iff => {
                                let a = Formula::And(
                                    lhs.clone().negation_normal_form_basic(),
                                    rhs.clone().negate().negation_normal_form_basic(),
                                );
                                let b = Formula::And(
                                    lhs.negate().negation_normal_form_basic(),
                                    rhs.negation_normal_form_basic(),
                                );
                                Formula::Or(a, b)
                            }
                        }
                    }

                    Quantified { q, var, fm } => {
                        let expr = std::mem::take(fm);
                        Formula::Quantified(
                            q.dual(),
                            var.clone(),
                            expr.negate().negation_normal_form_basic(),
                        )
                    }

                    _ => self,
                },
            },

            Binary { op, lhs, rhs } => {
                let lhs = std::mem::take(lhs);
                let rhs = std::mem::take(rhs);
                match op {
                    And => Formula::And(
                        lhs.negation_normal_form_basic(),
                        rhs.negation_normal_form_basic(),
                    ),
                    Or => Formula::Or(
                        lhs.negation_normal_form_basic(),
                        rhs.negation_normal_form_basic(),
                    ),
                    Imp => Formula::Or(
                        lhs.negate().negation_normal_form_basic(),
                        rhs.negation_normal_form_basic(),
                    ),
                    Iff => {
                        let a = Formula::And(
                            lhs.clone().negation_normal_form_basic(),
                            rhs.clone().negation_normal_form_basic(),
                        );
                        let b = Formula::And(
                            lhs.negate().negation_normal_form_basic(),
                            rhs.negate().negation_normal_form_basic(),
                        );
                        Formula::Or(a, b)
                    }
                }
            }

            Quantified { q, var, fm } => {
                let expr = std::mem::take(fm);
                Formula::Quantified(*q, var.clone(), expr.negation_normal_form_basic())
            }

            _ => self,
        }
    }

    pub fn negation_normal_form(self) -> Self {
        self.simplify().negation_normal_form_basic()
    }
}

impl<A: Atomic> Formula<A> {
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

                    Quantified { .. } => todo!(),

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

            Quantified { .. } => todo!(),

            _ => self,
        }
    }

    pub fn nenf(self) -> Self {
        self.simplify().nenf_basic()
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, parse_first_order, parse_propositional};

    #[test]
    fn nnf_simple() {
        let expr = parse_propositional("(p <=> q) <=> ~(r ==> s)");
        let expr_nnf = expr.clone().negation_normal_form();

        assert!(Formula::Iff(expr, expr_nnf).tautology())
    }

    #[test]
    fn nnf_quantifier() {
        let expr =
            parse_first_order("forall x. P(x) => (exists y. Q(y) <=> exists z. (P(z) & Q(z)))");

        let expected = parse_first_order(
            "exists x. ~P(x) | exists y. Q(y) & exists z. (P(z) & Q(z)) | forall y. ~Q(y) & forall z. (~P(z) | ~Q(z))",
        );

        let nnf = expr.negation_normal_form();

        assert_eq!(nnf, expected);
    }

    #[test]
    fn nenf_simple() {
        let expr = parse_propositional("(p <=> q) <=> ~(r ==> s)");
        let expr_nenf = expr.clone().nenf();

        assert!(Formula::Iff(expr, expr_nenf).tautology())
    }
}
