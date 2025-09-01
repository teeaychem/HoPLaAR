use crate::logic::{first_order::{Term, TermId}, Atomic};

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Relation {
    id: TermId,
    terms: Vec<Term>,
}

impl Relation {
    pub fn from(id: TermId, terms: Vec<Term>) -> Self {
        Self { id, terms }
    }
}

impl Relation {
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

impl std::fmt::Display for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.terms.as_slice() {
            [] => write!(f, "{}", self.id),
            [first, remaining @ ..] => {
                let mut term_string = format!("{first}");
                for term in remaining {
                    term_string.push_str(&format!(", {term}"));
                }

                write!(f, "({}, {term_string})", self.id)
            }
        }
    }
}

impl Atomic for Relation {
    fn id(&self) -> &str {
        &self.id
    }
}
