mod display;
mod parsing;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum OpUnary {
    Not,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum OpBinary {
    And,
    Or,
    Imp,
    Iff,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Quantifier {
    ForAll,
    Exists,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Formula<T: std::fmt::Debug + std::fmt::Display> {
    True,
    False,

    Atom {
        var: T,
    },

    OpUnary {
        op: OpUnary,
        expr: Box<Formula<T>>,
    },

    OpBinary {
        op: OpBinary,
        lhs: Box<Formula<T>>,
        rhs: Box<Formula<T>>,
    },

    Quantifier {
        q: Quantifier,
        var: T,
        expr: Box<Formula<T>>,
    },
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Prop {
    name: String,
}

impl Prop {
    pub fn new(name: &str) -> Self {
        Self { name: name.to_owned() }
    }
}

// Propositional formula
pub type PropFormula = Formula<Prop>;

#[allow(non_snake_case)]
impl PropFormula {
    pub fn Not(expr: PropFormula) -> Self {
        Self::OpUnary {
            op: OpUnary::Not,
            expr: Box::new(expr),
        }
    }

    pub fn And(lhs: PropFormula, rhs: PropFormula) -> Self {
        Self::OpBinary {
            op: OpBinary::And,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn Or(lhs: PropFormula, rhs: PropFormula) -> Self {
        Self::OpBinary {
            op: OpBinary::Or,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn Imp(lhs: PropFormula, rhs: PropFormula) -> Self {
        Self::OpBinary {
            op: OpBinary::Imp,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn Iff(lhs: PropFormula, rhs: PropFormula) -> Self {
        Self::OpBinary {
            op: OpBinary::Iff,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn Atom(prop: Prop) -> Self {
        Self::Atom { var: prop }
    }
}
