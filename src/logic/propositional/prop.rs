use std::convert::Infallible;

use crate::logic::{
    Atomic, Variable,
    first_order::{FirstOrderFormula, Relation},
    propositional::PropFormula,
};

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Prop {
    id: String,
}

impl Variable for Infallible {
    fn fmt_ansi(&self, f: &mut std::fmt::Formatter<'_>, ansi: bool) -> std::fmt::Result {
        todo!()
    }
}

impl Atomic for Prop {
    // Waiting for "!"
    type Part = Infallible;

    type Constant = Infallible;
    type Function = Infallible;
    type Variable = Infallible;

    fn id(&self) -> &str {
        &self.id
    }

    fn parts(&self) -> impl Iterator<Item = &Self::Part> {
        std::iter::empty()
    }

    fn constants(&self) -> impl Iterator<Item = &Self::Constant> {
        std::iter::empty()
    }

    fn functions(&self) -> impl Iterator<Item = &Self::Function> {
        std::iter::empty()
    }

    fn variables(&self) -> impl Iterator<Item = &Self::Variable> {
        std::iter::empty()
    }

    fn fmt_ansi(&self, f: &mut std::fmt::Formatter<'_>, _ansi: bool) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl From<&str> for Prop {
    fn from(value: &str) -> Self {
        Self {
            id: value.to_owned(),
        }
    }
}

impl From<&String> for Prop {
    fn from(value: &String) -> Self {
        Self {
            id: value.to_owned(),
        }
    }
}

impl Prop {
    pub fn name_set(&mut self, name: String) {
        self.id = name
    }
}

#[derive(Debug)]
pub struct PropSeq {
    id: String,
    idx: usize,
}

impl PropSeq {
    fn new(id: &str) -> Self {
        Self {
            id: String::from(id),
            idx: 0,
        }
    }
}

impl Default for PropSeq {
    fn default() -> Self {
        Self {
            id: String::from("p"),
            idx: 0,
        }
    }
}

impl Iterator for PropSeq {
    type Item = Prop;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == usize::MAX {
            None
        } else {
            let prop = Prop::from(&format!("{}_{}", self.id, self.idx));
            self.idx += 1;
            Some(prop)
        }
    }
}

impl std::fmt::Display for Prop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id())
    }
}

impl From<Relation> for Prop {
    fn from(value: Relation) -> Self {
        Prop::from(&value.to_string())
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::propositional::prop::PropSeq;

    #[test]
    fn prop_sequences() {
        let mut props = PropSeq::default();
        assert!(props.next().is_some_and(|p| format!("{p}") == "p_0"));
        props.nth(4);
        assert!(props.next().is_some_and(|p| format!("{p}") == "p_6"));

        let mut props = PropSeq::new("prop");
        assert!(props.next().is_some_and(|p| format!("{p}") == "prop_0"));
        props.nth(6);
        assert!(props.next().is_some_and(|p| format!("{p}") == "prop_8"));
    }
}
