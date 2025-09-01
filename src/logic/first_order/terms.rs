pub type TermId = String;

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum Term {
    /// A constant, or function with no arguments
    Cst { id: TermId },
    /// A variable
    Var { id: TermId },
    /// A function, with at least one argument
    Fun { id: TermId, args: Vec<Term> },
}

#[allow(non_snake_case)]
impl Term {
    pub fn Cst(id: TermId) -> Self {
        Term::Cst { id }
    }

    pub fn Fun(id: TermId, args: Vec<Term>) -> Self {
        Term::Fun {
            id,
            args,
        }
    }

    pub fn Var(id: &str) -> Self {
        Term::Var { id: id.to_owned() }
    }
}

impl Term {
    pub fn variable(id: &str) -> Self {
        Term::Var { id: id.to_owned() }
    }

    pub fn constant(id: &str) -> Self {
        Term::Cst { id: id.to_owned() }
    }

    pub fn function(id: &str, args: &[Term]) -> Self {
        Term::Fun {
            id: id.to_owned(),
            args: args.to_vec(),
        }
    }

    pub fn unary(op: &str, term: Term) -> Self {
        Term::function(op, &[term])
    }

    pub fn binary(op: &str, lhs: Term, rhs: Term) -> Self {
        Term::function(op, &[lhs, rhs])
    }

    pub fn is_const_id(id: &TermId) -> bool {
        match id.as_str() {
            "nil" => true,
            _ => id.chars().all(|c| c.is_numeric()),
        }
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Cst { id } => write!(f, "{id}"),
            Term::Var { id } => write!(f, "{id}"),
            Term::Fun { id, args } => match args.as_slice() {
                [] => write!(f, "{}", id),
                [first, remaining @ ..] => {
                    let mut arg_string = format!("{first}");
                    for arg in remaining {
                        arg_string.push_str(&format!(", {arg}"));
                    }

                    write!(f, "{}({arg_string})", id)
                }
            },
        }
    }
}
