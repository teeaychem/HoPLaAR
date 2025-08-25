use crate::logic::Formula;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
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

    pub fn name_set(&mut self, name: String) {
        self.name = name
    }
}

// Propositional formula
pub type PropFormula = Formula<Prop>;
