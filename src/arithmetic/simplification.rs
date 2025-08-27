use crate::arithmetic::Expr;

impl Expr {
    pub fn simplify_once(self) -> Self {
        use super::Expr::*;
        match &self {
            Var { .. } => self,

            Const { .. } => self,

            Add { lhs, rhs } => {
                //
                match (lhs.as_ref(), rhs.as_ref()) {
                    (Const { val: m }, Const { val: n }) => Const { val: m + n },
                    (Const { val: 0 }, _) => *rhs.clone(),
                    (_, Const { val: 0 }) => *lhs.clone(),
                    _ => self,
                }
            }

            Mul { lhs, rhs } => {
                //
                match (lhs.as_ref(), rhs.as_ref()) {
                    (Const { val: m }, Const { val: n }) => Const { val: m * n },

                    (Const { val: 0 }, _) => Const { val: 0 },
                    (Const { val: 1 }, _) => *rhs.clone(),

                    (_, Const { val: 0 }) => Const { val: 0 },
                    (_, Const { val: 1 }) => *lhs.clone(),

                    _ => self,
                }
            }
        }
    }

    pub fn simplify(self) -> Self {
        use super::Expr::*;

        match self {
            Add { lhs, rhs } => Add {
                lhs: Box::new(lhs.simplify()),
                rhs: Box::new(rhs.simplify()),
            }
            .simplify_once(),
            Mul { lhs, rhs } => Mul {
                lhs: Box::new(lhs.simplify()),
                rhs: Box::new(rhs.simplify()),
            }
            .simplify_once(),

            _ => self,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::arithmetic::Expr;

    #[test]
    fn one() {
        let fifteen_simple = Expr::Const(15);
        let fifteen_complex =
            (Expr::Const(0) * Expr::Var("x") + Expr::Const(1) * Expr::Const(3)) + Expr::Const(12);

        assert_eq!(fifteen_simple, fifteen_complex.simplify());

        let x_plus_twelve_simple = Expr::Var("x") + Expr::Const(12);
        let x_plus_twelve_complex =
            (Expr::Const(1) * Expr::Var("x") + Expr::Const(0) * Expr::Const(3)) + Expr::Const(12);

        assert_eq!(x_plus_twelve_simple, x_plus_twelve_complex.simplify());
    }
}
