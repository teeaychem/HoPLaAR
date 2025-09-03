use crate::logic::{
    Atomic,
    first_order::{Element, Model, Term, TermId, Valuation},
};

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Relation {
    id: TermId,
    terms: Vec<Term>,
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
    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn terms(&self) -> &[Term] {
        &self.terms
    }

    #[allow(non_snake_case)]
    pub fn eval<E: Element>(&self, I: &Model<E>, v: &Valuation<E>) -> bool {
        I.interpret_relation(self, v)
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
    fn id(&self) -> &str {
        &self.id
    }
}
