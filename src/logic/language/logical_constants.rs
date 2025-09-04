#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum OpUnary {
    Not,
}

impl std::fmt::Display for OpUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpUnary::Not => write!(f, "¬"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum OpBinary {
    And,
    Or,
    Imp,
    Iff,
}

impl std::fmt::Display for OpBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpBinary::And => write!(f, "∧"),
            OpBinary::Or => write!(f, "∨"),
            OpBinary::Imp => write!(f, "→"),
            OpBinary::Iff => write!(f, "↔"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Quantifier {
    ForAll,
    Exists,
}

impl Quantifier {
    pub fn dual(self) -> Self {
        match self {
            Self::ForAll => Self::Exists,
            Self::Exists => Self::ForAll,
        }
    }
}

impl std::fmt::Display for Quantifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Quantifier::ForAll => write!(f, "∀"),
            Quantifier::Exists => write!(f, "∃"),
        }
    }
}
