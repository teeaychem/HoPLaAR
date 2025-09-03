use crate::logic::{
    Atomic,
    first_order::{Element, Model, Term, TermId, Valuation, terms::Var},
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
}

impl std::fmt::Display for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.terms.as_slice() {
            [] => write!(f, "{}", self.id),
            [first, remaining @ ..] => {
                let mut term_string = format!("{first}");
                for term in remaining {
                    term_string.push_str(&format!(", {term}"));
                }

                write!(f, "{}({term_string})", self.id)
            }
        }
    }
}

impl Atomic for Relation {
    type Quantum = Var;

    fn id(&self) -> &str {
        &self.id
    }

    fn variables(&self) -> impl Iterator<Item = Self::Quantum> {
        self.terms
            .iter()
            .map(|term| term.variables().into_iter())
            .flatten()
    }
}
