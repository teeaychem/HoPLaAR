pub mod parsing;
pub mod simplification;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Var { id: String },
    Const { val: i64 },
    Add { lhs: Box<Expr>, rhs: Box<Expr> },
    Mul { lhs: Box<Expr>, rhs: Box<Expr> },
}

impl std::ops::Add for Expr {
    type Output = Expr;

    fn add(self, rhs: Self) -> Self::Output {
        Expr::Add {
            lhs: Box::new(self),
            rhs: Box::new(rhs),
        }
    }
}

impl std::ops::Mul for Expr {
    type Output = Expr;

    fn mul(self, rhs: Self) -> Self::Output {
        Expr::Mul {
            lhs: Box::new(self),
            rhs: Box::new(rhs),
        }
    }
}

#[allow(non_snake_case)]
impl Expr {
    pub fn Var(id: &str) -> Self {
        Expr::Var { id: id.to_owned() }
    }

    pub fn Const(val: i64) -> Self {
        Expr::Const { val }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expr::Var { id } => write!(f, "{}", id),
            Expr::Const { val } => write!(f, "{}", val),
            Expr::Add { lhs, rhs } => write!(f, "({} + {})", lhs, rhs),
            Expr::Mul { lhs, rhs } => write!(f, "({} * {})", lhs, rhs),
        }
    }
}
