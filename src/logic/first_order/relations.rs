use crate::logic::{
    Atomic,
    first_order::{
        Element, Model, Term, TermId, Valuation,
        terms::{Fun, Var},
    },
};

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Relation {
    pub id: TermId,
    pub terms: Vec<Term>,
}

impl Relation {
    pub fn from(id: TermId, terms: Vec<Term>) -> Self {
        Self { id, terms }
    }

    pub fn predicate(id: &str) -> Self {
        Self {
            id: id.to_owned(),
            terms: Vec::default(),
        }
    }

    pub fn n_ary(id: &str, terms: &[Term]) -> Self {
        Self {
            id: id.to_owned(),
            terms: terms.to_vec(),
        }
    }
}

impl Relation {
    #[allow(non_snake_case)]
    pub fn eval<E: Element, M: Model<E>>(&self, I: &M, v: &Valuation<E>) -> bool {
        I.relations(self, v)
    }

    pub fn string_ansi(&self) -> String {
        use std::fmt::Write;
        let mut s = String::default();
        let _ = write!(s, "{}", self.id);

        match self.terms.as_slice() {
            [] => {}
            [first, remaining @ ..] => {
                let _ = write!(s, "(");
                let _ = write!(s, "{}", first.string_ansi());
                for term in remaining {
                    let _ = write!(s, ", {}", term.string_ansi());
                }
                let _ = write!(s, ")");
            }
        }
        s
    }
}

impl std::fmt::Display for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        write!(f, "(")?;

        match self.terms.as_slice() {
            [] => {}
            [first, remaining @ ..] => {
                write!(f, "{first}")?;
                for term in remaining {
                    write!(f, ", {term}")?;
                }
            }
        }
        write!(f, ")")
    }
}

impl Atomic for Relation {
    type Part = Term;

    type Constant = Fun;
    type Function = Fun;
    type Variable = Var;

    fn id(&self) -> &str {
        &self.id
    }

    fn parts(&self) -> impl Iterator<Item = &Self::Part> {
        self.terms.iter().flat_map(|t| t.terms_d())
    }

    fn constants(&self) -> impl Iterator<Item = &Self::Constant> {
        self.parts().flat_map(|term| match term {
            Term::F(fun) => match fun.args.len() {
                0 => Some(fun),
                _ => None,
            },
            Term::V(_) => None,
        })
    }

    fn functions(&self) -> impl Iterator<Item = &Self::Function> {
        self.parts().flat_map(|term| match term {
            Term::F(fun) => match fun.args.len() {
                0 => None,
                _ => Some(fun),
            },
            Term::V(_) => None,
        })
    }

    fn variables(&self) -> impl Iterator<Item = &Self::Variable> {
        self.parts().flat_map(|term| match term {
            Term::F(_) => None,
            Term::V(var) => Some(var),
        })
    }
}
