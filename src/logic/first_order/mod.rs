#[derive(Clone)]
pub struct Var {
    id: String,
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Clone)]
pub struct Fun {
    id: String,
    args: Vec<Term>,
}

impl std::fmt::Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.args.as_slice() {
            [] => write!(f, "{}", self.id),
            [first, remaining @ ..] => {
                let mut arg_string = format!("{first}");
                for arg in remaining {
                    arg_string.push_str(&format!(", {arg}"));
                }

                write!(f, "{}({arg_string})", self.id)
            }
        }
    }
}

#[derive(Clone)]
pub enum Term {
    Var(Var),
    Fun(Fun),
}

impl Term {
    pub fn variable(id: &str) -> Self {
        Term::Var(Var { id: id.to_owned() })
    }

    pub fn constant(id: &str) -> Self {
        Term::Fun(Fun {
            id: id.to_owned(),
            args: Vec::default(),
        })
    }

    pub fn function(id: &str, args: &[Term]) -> Self {
        if args.is_empty() {
            panic!("Use Term::constant(...) to create a constant")
        }

        Term::Fun(Fun {
            id: id.to_owned(),
            args: args.to_vec(),
        })
    }

    pub fn unary(op: &str, term: Term) -> Self {
        Term::function(op, &[term])
    }

    pub fn binary(op: &str, lhs: Term, rhs: Term) -> Self {
        Term::function(op, &[lhs, rhs])
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Var(var) => write!(f, "{var}"),
            Term::Fun(fun) => write!(f, "{fun}"),
        }
    }
}

pub struct Relation {
    id: String,
    terms: Vec<Term>,
}

#[cfg(test)]
mod tests {
    use crate::logic::first_order::Term;

    #[test]
    fn debug() {
        let term = Term::unary(
            "sqrt",
            Term::binary(
                "-",
                Term::constant("1"),
                Term::unary(
                    "cos",
                    Term::binary(
                        "pow",
                        Term::binary("+", Term::variable("x"), Term::variable("y")),
                        Term::constant("2"),
                    ),
                ),
            ),
        );

        println!("{term}");
    }
}
