use crate::logic::first_order::{Element, Model, Valuation, eval_term};

pub type TermId = String;

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Fun {
    id: TermId,
    args: Vec<Term>,
}

impl Fun {
    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn args(&self) -> &[Term] {
        &self.args
    }
}

impl std::fmt::Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1B[1m{}\x1B[0m", self.id)?;

        match self.args.as_slice() {
            [] => {}
            [first, remaining @ ..] => {
                write!(f, "(")?;
                write!(f, "{first}")?;
                for arg in remaining {
                    write!(f, ", {arg}")?;
                }
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Var {
    id: TermId,
}

impl Var {
    pub fn from(id: &str) -> Self {
        Var { id: id.to_owned() }
    }

    pub fn id(&self) -> &str {
        &self.id
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1B[3m{}\x1B[0m", self.id)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum Term {
    /// A function
    F(Fun),
    /// A variable
    V(Var),
}

#[allow(non_snake_case)]
impl Term {
    pub fn cst(id: &str) -> Self {
        Term::F(Fun {
            id: id.to_owned(),
            args: Vec::default(),
        })
    }

    pub fn fun(id: &str, args: Vec<Term>) -> Self {
        Term::F(Fun {
            id: id.to_owned(),
            args,
        })
    }

    pub fn var(id: &str) -> Self {
        Term::V(Var { id: id.to_owned() })
    }

    pub fn fun_slice(id: &str, args: &[Term]) -> Self {
        Term::F(Fun {
            id: id.to_owned(),
            args: args.to_vec(),
        })
    }
}

impl Term {
    pub fn id(&self) -> &str {
        match self {
            Term::V(Var { id }) | Term::F(Fun { id, .. }) => id,
        }
    }

    pub fn unary(op: &str, term: Term) -> Self {
        Term::fun_slice(op, &[term])
    }

    pub fn binary(op: &str, lhs: Term, rhs: Term) -> Self {
        Term::fun_slice(op, &[lhs, rhs])
    }

    pub fn is_const_id(id: &TermId) -> bool {
        match id.as_str() {
            "nil" => true,
            _ => id.chars().all(|c| c.is_numeric()),
        }
    }

    pub fn to_variable(self) -> Var {
        match self {
            Term::V(var) => var,
            Term::F(Fun { id, args }) => {
                if args.is_empty() {
                    Var { id }
                } else {
                    panic!("Attempt to convert {id} to variable failed")
                }
            }
        }
    }

    #[allow(non_snake_case)]
    pub fn eval<E: Element, M: Model<E>>(&self, I: &M, v: &Valuation<E>) -> E {
        eval_term(self, I, v)
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::V(var) => write!(f, "{var}"),
            Term::F(fun) => write!(f, "{fun}"),
        }
    }
}

pub struct TermIteratorD<'a> {
    stack: Vec<&'a Term>,
    expr: Option<&'a Term>,
}

impl Term {
    // An iterator producing the given term and all sub-terms.
    pub fn terms_d(&'_ self) -> TermIteratorD<'_> {
        TermIteratorD {
            stack: Vec::default(),
            expr: Some(self),
        }
    }
}

impl<'a> Iterator for TermIteratorD<'a> {
    type Item = &'a Term;

    fn next(&mut self) -> Option<Self::Item> {
        match self.expr {
            Some(Term::V(_)) => {
                let var = self.expr;
                self.expr = self.stack.pop();
                var
            }

            Some(Term::F(Fun { args, .. })) => {
                let fun = self.expr;
                self.stack.extend(args.iter().rev());
                self.expr = self.stack.pop();
                fun
            }

            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::parse::try_parse_term;

    #[test]
    fn term_variables() {
        let terms = try_parse_term("f(a,g(b),h(f(a,h(b),c,d)))")
            .unwrap()
            .terms_d()
            .count();
        assert_eq!(terms, 11);
    }
}
