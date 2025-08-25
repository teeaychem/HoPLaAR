use crate::logic::{Formula, OpBinary, OpUnary, Quantifier, propositional::Prop};

impl std::fmt::Display for OpUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpUnary::Not => write!(f, "¬"),
        }
    }
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

impl std::fmt::Display for Quantifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Quantifier::ForAll => write!(f, "∀"),
            Quantifier::Exists => write!(f, "∃"),
        }
    }
}

impl<T: std::fmt::Debug + std::fmt::Display + Clone> std::fmt::Display for Formula<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Formula::True => write!(f, "⊤"),
            Formula::False => write!(f, "⊥"),
            Formula::Atom { var } => write!(f, "{var}"),
            Formula::OpUnary { op, expr } => write!(f, "{op}({expr})"),
            Formula::OpBinary { op, lhs, rhs } => write!(f, "({lhs} {op} {rhs})"),
            Formula::Quantifier { q, var: atom, expr } => write!(f, "{q}{atom}({expr})"),
        }
    }
}

impl std::fmt::Display for Prop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
