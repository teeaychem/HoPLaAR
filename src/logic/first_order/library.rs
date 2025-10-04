// Problems from: Pelletier (1986) Seventy-five problems for testing automatic theorem provers.

pub mod pelletier {
    pub use {monadic_predicate::*, propositional::*};

    pub mod propositional {
        pub const P1: &str = "(p → q) ↔ (q → ¬p)";

        pub const P2: &str = "¬p ↔ p";

        pub const P3: &str = "¬(p → q) → (q → p)";

        pub const P4: &str = "(¬p → q) ↔ (¬q → p)";

        pub const P5: &str = "((p ∨ q) → (p ∨ r)) → (p ∨ (q → r))";

        pub const P6: &str = "p ∨ ¬p";

        pub const P7: &str = "p ∨ ¬p";

        pub const P8: &str = "((p → q) → p) → p";

        pub const P9: &str = "[(p ∨ q) ∧ (¬p ∨ q) ∧ (p ∨ ¬q)] → ¬(¬p ∨ ¬q)";

        pub const P10: &str = "(q → r) ∧ (r → (p ∧ q)) ∧ (p → (q ∨ r)) → (p ↔ q)";

        pub const P11: &str = "p ↔ p";

        pub const P12: &str = "[(p ↔ q) ↔ r] ↔ [p ↔ (q ↔ r)]";

        pub const P13: &str = "[p ∨ (q ∧ r)] ↔ [(p ∨ q) ∧ (p ∨ r)]";

        pub const P14: &str = "(p ↔ q) ↔ ((q ∨ ¬p) ∧ (¬q ∨ p))";

        pub const P15: &str = "(p → q) ↔ (¬p ∨ q)";

        pub const P16: &str = "(p → q) ∨ (q → p)";

        pub const P17: &str = "((p ∧ (q → r)) → s) ↔ ((¬p ∨ q ∨ s) ∧ (¬p ∨ ¬r ∨ s))";
    }

    pub mod monadic_predicate {

        pub const P18: &str = "∃y. ∀x. (F(y) → F(x))";

        pub const P19: &str = "∃x. ∀y. ∀z. ((P(y) → Q(z)) → (P(x) → Q(x)))";

        pub const P20: &str = "∀x. (∀y. (∃z. (∀w. ((P(x) ∧ Q(y)) → (R(z) ∧ U(w))) → (∃x. (∃y. ((P(x) ∧ Q(y)) → ∃z. (R(z))))))))";

        pub const P21: &str = "∃x. (p → F(x)) ∧ ∃x. (F(x) → p) → ∃x. (p ↔ F(x))";

        pub const P22: &str = "∀x. (p ↔ F(x)) ↔ (p ↔ ∀x. F(x))";

        pub const P23: &str = "∀x.(p ∨ F(x)) ↔ (p ∨ ∀x. F(x))";

        pub const P24: &str = "
  ¬(∃x. (U(x) ∧ Q(x))) ∧ (∀x. (P(x) → Q(x) ∨ R(x))) ∧ ¬(∃x. (P(x) → (∃x. Q(x))))∧ (∀x. (Q(x) ∧ R(x) → U(x)))
→ ∃x. (P(x) ∧ R(x))
";

        pub const P38: &str = "
∀x. (P(a) ∧ (P(x) → ∃y. (P(y) ∧ R(x,y))) → ∃z. ∃w. (P(z) ∧ R(x,w) ∧ R(w,z)))
↔
∀x. ((¬P(a) ∨ P(x) ∨ ∃z. ∃w. (P(z) ∧ R(x,w) ∧ R(w,z)))
     ∧ (¬P(a) ∨ ¬∃y. (P(y) ∧ R(x,y)) ∨ ∃z. ∃w. (P(z) ∧ R(x,w) ∧ R(w,z))))
";

        pub const P45: &str = "
  ∀x. (F(x) ∧ ∀y. [G(y) ∧ H(x, y) → J(x, y)] → ∀y. (G(y) ∧ H(x,y) → K(y)))
∧ ¬∃y. (L(y) ∧ K(y))
∧ ∃x. [F(x) ∧ ∀y. (H(x,y) → L(y)) ∧ ∀y. (G(y) ∧ H(x,y) → J(x,y))]
→
  ∃x. (F(x) ∧ ¬∃y. (G(y) ∧ H(x,y)))
";
    }
}

#[allow(non_upper_case_globals)]
pub mod satisfiable {
    pub const AxPxQx: &str = "∀x. P(x) ∨ Q(x)";

    pub const AxAyPxQy: &str = "∀x. ∀y. (P(x) ∨ Q(y))";

    pub const AxEyPxQx: &str = "∀x. ∃y. (P(x) → ¬P(y))";
}
