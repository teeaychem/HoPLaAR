// Problems from: Pelletier (1986) Seventy-five problems for testing automatic theorem provers.
pub mod pelletier {
    pub const P18: &str = "
exists y. forall x. (F(y) => F(x))";

    pub const P19: &str = "
exists y. forall x. forall z. ((P(y) => Q(z)) => (P(x) => Q(x)))";

    pub const P20: &str = "
forall x.
  (forall y.
    (exists z.
      (forall w.
        ((P(x) & Q(y)) => (R(z) & U(w))) =>
          (exists x.
            (exists y. ((P(x) & Q(y)) => exists z. (R(z))))))))";

    pub const P24: &str = "
~(exists x. (U(x) & Q(x)))
& (forall x. (P(x) ==> Q(x) | R(x)))
& ~(exists x. (P(x) ==> (exists x. Q(x))))
& (forall x. (Q(x) & R(x) ==> U(x)))
=>
exists x. (P(x) & R(x))
";

    pub const P45: &str = "
forall x. (F(x) & forall y. [G(y) & H(x, y) => J(x, y)] => forall y. (G(y) & H(x,y) => K(y)))
& ~exists y. (L(y) & K(y))
& exists x. [F(x) & forall y. (H(x,y) => L(y)) & forall y. (G(y) & H(x,y) => J(x,y))]
=> exists x. (F(x) & ~exists y. (G(y) & H(x,y)))
";
}
