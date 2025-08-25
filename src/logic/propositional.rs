use crate::logic::Formula;

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Prop {
    name: String,
}

impl Prop {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

// Propositional formula
pub type PropFormula = Formula<Prop>;
