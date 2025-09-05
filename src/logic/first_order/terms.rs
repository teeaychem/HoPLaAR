use std::collections::HashSet;

use crate::logic::first_order::{Element, Model, Valuation, eval_term};

pub type TermId = String;

/// Functions (and constants)
///
/// As functions are always applied,
#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Fun {
    pub id: TermId,
    pub variant: usize,
    pub args: Vec<Term>,
}

impl Fun {
    /// Returns true if `self` and `other` are the same function.
    ///
    /// Definitional equality is distinct from equality between instances of Fun, as instances of Fun specify the arguments passed to a function definition.
    pub fn definitionally_eq(&self, other: &Fun) -> bool {
        self.id == other.id && self.args.len() == other.args.len()
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
    pub id: TermId,
    pub variant: usize,
}

impl From<&str> for Var {
    fn from(value: &str) -> Self {
        let (id, variant) = id_and_variant(value);
        Var { id, variant }
    }
}

impl Var {
    pub fn variant(&self, taken: &HashSet<Var>) -> Var {
        let mut minimal_variant = None;

        for var in taken {
            if var.id == self.id {
                if let Some(minimal) = minimal_variant {
                    minimal_variant = Some(std::cmp::max(minimal, var.variant) + 1);
                } else {
                    minimal_variant = Some(1)
                }
            }
        }

        Var {
            id: self.id.clone(),
            variant: minimal_variant.unwrap_or_default(),
        }
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1B[3m{}\x1B[0m", self.id)?;
        if 0 < self.variant {
            write!(f, "\x1B[3m_{}\x1B[0m", self.variant)?
        }
        Ok(())
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
    pub fn Cst(value: &str) -> Self {
        let (id, variant) = id_and_variant(value);
        Term::F(Fun {
            id,
            variant,
            args: Vec::default(),
        })
    }

    pub fn Var(id: &str) -> Self {
        Term::V(Var::from(id))
    }

    pub fn Fun(str_id: &str, args: &[Term]) -> Self {
        let (id, variant) = id_and_variant(str_id);
        Term::F(Fun {
            id,
            variant,
            args: args.to_vec(),
        })
    }

    pub fn Fun_unary(str_id: &str, term: Term) -> Self {
        Term::Fun(str_id, &[term])
    }

    pub fn Fun_binary(str_id: &str, lhs: Term, rhs: Term) -> Self {
        Term::Fun(str_id, &[lhs, rhs])
    }
}

impl Term {
    pub fn id(&self) -> &str {
        match self {
            Term::V(Var { id, .. }) | Term::F(Fun { id, .. }) => id,
        }
    }

    pub fn variant(&self) -> usize {
        match self {
            Term::V(Var { variant, .. }) | Term::F(Fun { variant, .. }) => *variant,
        }
    }

    #[allow(non_snake_case)]
    pub fn eval<E: Element, M: Model<E>>(&self, M: &M, v: &Valuation<E>) -> E {
        eval_term(self, M, v)
    }

    pub fn variables(&self) -> HashSet<Var> {
        let mut vars = HashSet::default();

        for term in self.terms_d() {
            match term {
                Term::F(_) => {}
                Term::V(var) => {
                    vars.insert(var.to_owned());
                }
            }
        }

        vars
    }

    pub fn functions(&self) -> HashSet<Fun> {
        let mut vars = HashSet::default();

        for term in self.terms_d() {
            match term {
                Term::F(fun) => {
                    vars.insert(fun.to_owned());
                }
                Term::V(_) => {}
            }
        }

        vars
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

/// Splits a trailing `_[\d+]` from an otherwise non-empty `value` and returns the prefix and digits as a pair.
pub fn id_and_variant(value: &str) -> (String, usize) {
    let mut parts = value.split('_').peekable();
    let mut variant = 0;

    let mut base_id = String::default();
    while let Some(part) = parts.next() {
        if parts.peek().is_none()
            && !base_id.is_empty()
            && let Ok(id_variant) = part.parse::<usize>()
        {
            variant = id_variant;
            break;
        }

        base_id.push_str(part);
    }

    (base_id, variant)
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::{
        first_order::{Term, syntax::Substitution, terms::Var},
        parse::try_parse_term,
    };

    #[test]
    fn ids_and_variants() {
        let v23 = Var::from("v_23");
        assert_eq!(v23.id, "v");
        assert_eq!(v23.variant, 23);

        let one = Term::Cst("1");
        assert_eq!(one.id(), "1");
        assert_eq!(one.variant(), 0);

        let one = Term::Cst("1_1");
        assert_eq!(one.id(), "1");
        assert_eq!(one.variant(), 1);
    }

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
                        .id
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

        let substitution = Substitution::from_function(Box::new(upper_sub));

        let new_term = substitution.apply(term);

        let other_term = try_parse_term("f(x,g(X,y))").unwrap();

        assert_eq!(new_term, other_term);
    }

    #[test]
    fn variants() {
        let var = Var::from("x");

        let taken = HashSet::from(["y", "z"].map(Var::from));
        assert_eq!(var.variant(&taken), Var::from("x"));

        let taken = HashSet::from(["x", "y"].map(Var::from));
        assert_eq!(var.variant(&taken), Var::from("x_1"));

        let taken = HashSet::from(["x", "x_1"].map(Var::from));
        assert_eq!(var.variant(&taken), Var::from("x_2"));
    }
}
