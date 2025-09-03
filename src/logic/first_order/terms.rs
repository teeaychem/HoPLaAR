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

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::V(var) => write!(f, "{var}"),
            Term::F(fun) => write!(f, "{fun}"),
        }
    }
}

#[allow(non_snake_case)]
impl Term {
    pub fn Cst(id: &str) -> Self {
        Term::F(Fun {
            id: id.to_owned(),
            args: Vec::default(),
        })
    }

    pub fn Var(id: &str) -> Self {
        Term::V(Var { id: id.to_owned() })
    }

    pub fn Fun(id: &str, args: &[Term]) -> Self {
        Term::F(Fun {
            id: id.to_owned(),
            args: args.to_vec(),
        })
    }

    pub fn Fun_unary(op: &str, term: Term) -> Self {
        Term::Fun(op, &[term])
    }

    pub fn Fun_binary(op: &str, lhs: Term, rhs: Term) -> Self {
        Term::Fun(op, &[lhs, rhs])
    }
}

impl Term {
    pub fn id(&self) -> &str {
        match self {
            Term::V(Var { id }) | Term::F(Fun { id, .. }) => id,
        }
    }

    #[allow(non_snake_case)]
    pub fn eval<E: Element, M: Model<E>>(&self, M: &M, v: &Valuation<E>) -> E {
        eval_term(self, M, v)
    }

    pub fn substitute<S: Fn(Term) -> Term>(self, substitution: &S) -> Self {
        match self {
            Term::F(Fun { id, args }) => {
                let x: Vec<Term> = args
                    .into_iter()
                    .map(|arg| arg.substitute(substitution))
                    .collect();
                Term::Fun(&id, &x)
            }
            Term::V(_) => substitution(self),
        }
    }
}

pub struct TermIteratorD<'a> {
    stack: Vec<&'a Term>,
    expr: Option<&'a Term>,
}

impl Term {
    // An iterator of the term and all sub-terms.
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
    use crate::logic::{first_order::Term, parse::try_parse_term};

    #[test]
    fn term_variables() {
        let terms = try_parse_term("f(a,g(b),h(f(a,h(b),c,d)))")
            .unwrap()
            .terms_d()
            .count();
        assert_eq!(terms, 11);
    }

    #[test]
    fn substitution() {
        let term = try_parse_term("f(X,g(x,Y))").unwrap();
        let upper_sub = |t: Term| -> Term {
            match t {
                Term::F(_) => t,
                Term::V(var) => {
                    let recase: String = var
                        .id()
                        .chars()
                        .map(|c| {
                            if c.is_uppercase() {
                                c.to_lowercase().next().unwrap()
                            } else if c.is_lowercase() {
                                c.to_uppercase().next().unwrap()
                            } else {
                                c
                            }
                        })
                        .collect();

                    Term::Var(&recase)
                }
            }
        };
        let new_term = term.substitute(&upper_sub);

        let other_term = try_parse_term("f(x,g(X,y))").unwrap();

        assert_eq!(new_term, other_term);
    }
}
