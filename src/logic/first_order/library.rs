// Problems from: Pelletier (1986) Seventy-five problems for testing automatic theorem provers.

pub mod pelletier {
    pub use {full_predicate::*, monadic_predicate::*, propositional::*};

    pub mod propositional {
        pub const P1: &str = "(p → q) ↔ (¬q → ¬p)";

        pub const P2: &str = "¬¬p ↔ p";

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

        pub const P18: &str = "∃y.∀x.(F(y) → F(x))";

        pub const P19: &str = "∃x.∀y.∀z.((P(y) → Q(z)) → (P(x) → Q(x)))";

        pub const P20: &str = "∀x.(∀y.(∃z.(∀w.((P(x) ∧ Q(y)) → (R(z) ∧ U(w))) → (∃x.(∃y.((P(x) ∧ Q(y)) → ∃z.(R(z))))))))";

        pub const P21: &str = "∃x.(p → F(x)) ∧ ∃x.(F(x) → p) → ∃x.(p ↔ F(x))";

        pub const P22: &str = "∀x.(p ↔ F(x)) ↔ (p ↔ ∀x.F(x))";

        pub const P23: &str = "∀x.(p ∨ F(x)) ↔ (p ∨ ∀x.F(x))";

        pub const P24: &str = "¬(∃x.(U(x) ∧ Q(x))) ∧ (∀x.(P(x) → Q(x) ∨ R(x))) ∧ ¬(∃x.(P(x) → (∃x.Q(x))))∧ (∀x.(Q(x) ∧ R(x) → U(x)))
→ ∃x.(P(x) ∧ R(x))";

        pub const P25: &str = "
∃x.P(x) ∧ ∀x.(F(x) → (¬G(x) ∧ R(x))) ∧ ∀x.(P(x) → (G(x) ∧ F(x))) ∧ [∀x.(P(x) → Q(x)) ∨ ∃x.(P(x) ∧ R(x))] → ∃x.(Q(x) ∧ P(x))";

        pub const P26: &str = "[∃x.P(x) ↔ ∃x.Q(x)] ∧ ∀x.∀y.(P(x) ∧ Q(y) → (R(x) ↔ S(y))) → [∀x.(P(x) → R(x)) ↔ ∀x.(Q(x) → S(x))]";

        pub const P27: &str = "∃x.(F(x) ∧ ¬G(x)) ∧ ∀x.(F(x) → H(x)) ∧ ∀x.(J(x) ∧ I(x) → F(x)) ∧ ∃x.(H(x) ∧ ¬G(x)) → ∀x.(I(x) → ¬H(x)) → ∀x.(J(x) → ¬I(x))";

        pub const P28: &str = "[[∀x.P(x) → ∀x.Q(x)] ∧ [∀x.(Q(x) ∨ R(x)) → ∃x.(Q(x) ∧ S(x))] ∧ [∃x.S(x) → ∀x.(F(x) → G(x))]] → ∀x.((P(x) ∧ F(x)) → G(x))";

        pub const P29: &str = "∃x.F(x) ∧ ∃x.G(x) → [∀x.(F(x) → H(x)) ∧ ∀x.(G(x) → J(x))] ↔ ∀x.∀y.(F(x) ∧ G(y) → H(x) ∧ J(y))";

        pub const P30: &str =
            "∀x.(F(x) ∨ G(x) → ¬H(x)) ∧ ∀x.((G(x) → ¬I(x)) → F(x) ∧ H(x)) → ∀x.I(x)";

        pub const P31: &str =
            "¬∃x.(F(x) ∧ (G(x) ∨ H(x))) ∧ ∃x.(I(x) ∧ F(x)) ∧ ∀x.(¬H(x) → J(x)) → ∃x.(I(x) ∧ J(x))";

        pub const P32: &str = "[[∀x.(F(x) ∧ (G(x) ∨ H(x)) → I(x))] ∧ ∀x.(I(x) ∧ F(x) → J(x)) ∧ ∀x.(K(x) → H(x))] → ∀x.(F(x) ∧ K(x) → J(x))";

        pub const P33: &str = "∀x.(P(a) ∧ (P(x) → P(b)) → P(c)) ↔ ∀x.(¬P(a) ∨ (P(x) ∨ P(c)) ∧ (¬P(a) ∨ (¬P(b) ∨ P(c))))";

        pub const P34: &str = "[∃x.∀y.(P(x) ↔ P(y)) ↔ (∃x.Q(x) ↔ ∀y.P(y))] ↔ [∃x.∀y.(Q(x) ↔ Q(y)) ↔ (∃x.P(x) ↔ ∀y.Q(y))]";
    }

    pub mod full_predicate {

        pub const P35: &str = "∃x.∃y.(P(x,y) → ∀x.∀y.P(x,y))";

        pub const P36: &str = "[∀x.∃y.F(x,y) ∧ ∀x.∃y.G(x,y) ∧ ∀x.∀y.(F(x,y) ∨ G(x,y) → ∀z.(F(y,z) ∨ G(y,z) → H(x,z)))] → ∀x.∃y.H(x,y)";

        pub const P37: &str = "∀z.∃w.∀x.∃y.[(P(x,z) → P(y,w)) ∧ P(y,z) ∧ (P(y,w) → ∃u.(Q(u,w)))] ∧ ∀x ∀z.(¬P(x,z) → ∃y.Q(y,z)) ∧ [∃x.∃y.Q(x,y) → ∀x.R(x,x)] → ∀x.∃y.R(x,y)";

        pub const P38: &str = "∀x.(P(a) ∧ (P(x) → ∃y.(P(y) ∧ R(x,y))) → ∃z.∃w.(P(z) ∧ R(x,w) ∧ R(w,z))) ↔ ∀x.((¬P(a) ∨ P(x) ∨ ∃z.∃w.(P(z) ∧ R(x,w) ∧ R(w,z))) ∧ (¬P(a) ∨ ¬∃y.(P(y) ∧ R(x,y)) ∨ ∃z.∃w.(P(z) ∧ R(x,w) ∧ R(w,z))))";

        pub const P39: &str = "¬∃x.∀y.(F(y,x) ↔ ¬F(y,y))";

        pub const P40: &str = "∃y.∀x.(F(x,y) ↔ F(x,x)) → ¬∀x.∃y.∀z.(F(x,y) ↔ ¬F(z,x))";

        pub const P41: &str = "∀z.∃y.∀x.(F(x,y) ↔ F(x,z) ∧ ¬F(x,x)) → ¬∃z.∀x.F(x,z)";

        pub const P42: &str = "¬∃y.∀x.(F(x,y) ↔ ¬∃z.(F(x,z) ∧ F(z,x)))";

        pub const P43: &str = "∀x.∀y.(Q(x,y) ↔ ∀z.(F(z,x) ↔ F(z,y))) → ∀x.∀y.(Q(x,y) ↔ Q(y,x))";

        pub const P44: &str = "∀x.[F(x) → ∃y.(G(y) ∧ H(x,y)) ∧ ∃y.(G(y) ∧ ¬H(x,y))] ∧ ∃x.[J(x) ∧ ∀y.[G(y) → H(x,y)]] → ∃x.(J(x) ∧ ¬F(x))";

        pub const P45: &str = "[∀x.(F(x) ∧ ∀y.[G(y) ∧ H(x, y) → J(x, y)] → ∀y.(G(y) ∧ H(x,y) → K(y))) ∧ ¬∃y.(L(y) ∧ K(y)) ∧ ∃x.[F(x) ∧ ∀y.(H(x,y) → L(y)) ∧ ∀y.(G(y) ∧ H(x,y) → J(x,y))]] → ∃x.(F(x) ∧ ¬∃y.(G(y) ∧ H(x,y)))";

        pub const P46: &str = "∀x.(F(x) ∧ ∀y.[F(y) ∧ H(y,x) → G(y)] → G(x)) ∧ ∃x.(F(x) ∧ ¬G(x) ∧ ∀y.(F(y) ∧ ¬G(y) → J(x,y))) ∧ ∀x.∀y.(F(x) ∧ F(y) ∧ H(x,y) → ¬J(x,y)) → ∀x.(F(x) → G(x))";

        pub const P47: &str = "
[  ∀x.(P_1(x) → P_0(x)) ∧ ∃x.P_1(x)
 ∧ ∀x.(P_2(x) → P_0(x)) ∧ ∃x.P_2(x)
 ∧ ∀x.(P_3(x) → P_0(x)) ∧ ∃x.P_3(x)
 ∧ ∀x.(P_4(x) → P_0(x)) ∧ ∃x.P_4(x)
 ∧ ∀x.(P_5(x) → P_0(x)) ∧ ∃x.P_5(x)
 ∧ ∃x.Q_1(x) ∧ ∀x.(Q_1(x) → Q_0(x))
 ∧ ∀x.(P_0(x) → [∀y.(Q_0(y) → R(x,y)) ∨ ∀y.((P_0(y) ∧ S(y,x) ∧ ∃z.(Q_0(z) ∧ R(y,z))) → R(x,y))])
 ∧ ∀x. ∀y.((P_3(x) ∧ P_2(y)) → S(x,y))
 ∧ ∀x. ∀y.((P_2(x) ∧ P_1(y)) → S(x,y))
 ∧ ∀x. ∀y.[(P_1(x) ∧ (P_2(y) ∨ Q_1(y))) → ~R(x,y)]
 ∧ ∀x. ∀y.((P_3(x) ∧ P_4(y)) → R(x,y))
 ∧ ∀x. ∀y.((P_3(x) ∧ P_5(y)) → ~R(x,y))
 ∧ ∀x.((P_4(x) ∨ P_5(x)) → ∃y.(Q_0(y) ∧ R(x,y)))
] → ∃x. ∃y. (P_0(x) ∧ P_0(y) ∧ ∃z.(Q_1(z) ∧ R(y,z) ∧ R(x,y)))
";
    }
}

#[allow(non_upper_case_globals)]
pub mod satisfiable {
    pub const AxPxQx: &str = "∀x.P(x) ∨ Q(x)";

    pub const AxAyPxQy: &str = "∀x.∀y.(P(x) ∨ Q(y))";

    pub const AxEyPxQx: &str = "∀x.∃y.(P(x) → ¬P(y))";
}
