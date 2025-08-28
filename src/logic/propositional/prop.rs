use crate::logic::Atomic;

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Prop {
    name: String,
}

impl Atomic for Prop {}

impl Prop {
    pub fn from(name: &str) -> Self {
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
