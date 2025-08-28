use crate::logic::{Atomic, Formula, OpBinary, OpUnary, Quantifier, propositional::Prop};

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

impl<A: Atomic> std::fmt::Display for Formula<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Formula::True => write!(f, "⊤"),
            Formula::False => write!(f, "⊥"),

            Formula::Atom { var } => write!(f, "{var}"),

            Formula::Unary { op, expr } => match expr.as_ref() {
                Formula::True
                | Formula::False
                | Formula::Atom { .. }
                | Formula::Unary { .. }
                | Formula::Quantifier { .. } => write!(f, "{op}{expr}"),

                Formula::Binary { .. } => write!(f, "{op}({expr})"),
            },

            Formula::Binary { op, lhs, rhs } => match (lhs.as_ref(), rhs.as_ref()) {
                (Formula::Binary { op: lop, .. }, Formula::Binary { op: rop, .. }) => {
                    if op == lop && op == rop {
                        write!(f, "{lhs} {op} {rhs}")
                    } else if op == lop {
                        write!(f, "{lhs} {op} ({rhs})")
                    } else if op == rop {
                        write!(f, "({lhs}) {op} {rhs}")
                    } else {
                        write!(f, "({lhs}) {op} ({rhs})")
                    }
                }

                (Formula::Binary { op: lop, .. }, _) => {
                    if op == lop {
                        write!(f, "{lhs} {op} {rhs}")
                    } else {
                        write!(f, "({lhs}) {op} {rhs}")
                    }
                }

                (_, Formula::Binary { op: rop, .. }) => {
                    if op == rop {
                        write!(f, "{lhs} {op} {rhs}")
                    } else {
                        write!(f, "{lhs} {op} ({rhs})")
                    }
                }

                _ => write!(f, "{lhs} {op} {rhs}"),
            },

            Formula::Quantifier { q, var: atom, expr } => write!(f, "{q}{atom}({expr})"),
        }
    }
}

impl std::fmt::Display for Prop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
