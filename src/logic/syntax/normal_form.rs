use crate::logic::{Atomic, Formula, OpBinary, OpUnary};

impl<A: Atomic> Formula<A> {
    pub fn negation_normal_form_basic(mut self) -> Self {
        use {Formula::*, OpBinary::*, OpUnary::*};

        match &mut self {
            Unary { op, fml } => match op {
                Not => match fml.as_mut() {
                    Unary { op, fml } => match op {
                        Not => std::mem::take(fml).negation_normal_form_basic(),
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

                    Quantified { q, var, fml: fm } => {
                        let fml = std::mem::take(fm);
                        Formula::Quantified(
                            q.dual(),
                            var.clone(),
                            fml.negate().negation_normal_form_basic(),
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
                        let a = Formula::Or(
                            lhs.clone().negate().negation_normal_form_basic(),
                            rhs.clone().negation_normal_form_basic(),
                        );

                        let b = Formula::Or(
                            lhs.negation_normal_form_basic(),
                            rhs.negate().negation_normal_form_basic(),
                        );

                        Formula::And(a, b)
                    }
                }
            }

            Quantified { q, var, fml: fm } => {
                let fml = std::mem::take(fm);
                Formula::Quantified(*q, var.clone(), fml.negation_normal_form_basic())
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
            Unary { op, fml } => match op {
                Not => match fml.as_mut() {
                    Unary { op, fml } => match op {
                        Not => std::mem::take(fml).nenf_basic(),
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

                    Quantified { q, var, fml: fm } => {
                        let fml = std::mem::take(fm);
                        Formula::Quantified(q.dual(), var.clone(), fml.negate().nenf_basic())
                    }

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

            Quantified { q, var, fml: fm } => {
                let fml = std::mem::take(fm);
                Formula::Quantified(*q, var.clone(), fml.negation_normal_form_basic())
            }

            _ => self,
        }
    }

    pub fn nenf(self) -> Self {
        self.simplify().nenf_basic()
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Formula, first_order::FirstOrderFormula, parse_propositional};

    #[test]
    fn nnf_simple() {
        let fml = parse_propositional("(p ↔ q) ↔ ¬(r → s)");
        let fml_nnf = fml.clone().negation_normal_form();

        assert!(Formula::Iff(fml, fml_nnf).is_tautology())
    }

    #[test]
    fn nnf_quantifier() {
        let fml = FirstOrderFormula::from("∀x.P(x) → (∃y.Q(y) ↔ ∃z.(P(z) & Q(z)))");

        let expected = FirstOrderFormula::from(
            "∃x.¬P(x) ∨ ((∀y.¬Q(y) ∨ ∃z.(P(z) ∧ Q(z))) ∧ (∃y.Q(y) ∨ ∀z.(¬P(z) ∨ ¬Q(z))))",
        );

        let nnf = fml.negation_normal_form();

        assert_eq!(nnf, expected);
    }

    #[test]
    fn nenf_simple() {
        let fml = parse_propositional("(p ↔ q) ↔ ¬(r → s)");
        let fml_nenf = fml.clone().nenf();

        assert!(Formula::Iff(fml, fml_nenf).is_tautology())
    }
}
