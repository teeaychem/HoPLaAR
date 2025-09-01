use crate::logic::Atomic;

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Prop {
    id: String,
}

impl Atomic for Prop {
    fn id(&self) -> &str {
        &self.id
    }
}

impl Prop {
    pub fn from(id: &str) -> Self {
        Self { id: id.to_owned() }
    }

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

#[cfg(test)]
mod tests {
    use crate::logic::propositional::prop::PropSeq;

    #[test]
    fn prop_sequences() {
        let mut props = PropSeq::default();
        assert_eq!(format!("{}", props.next().unwrap()), "p_0");
        props.nth(4);
        assert_eq!(format!("{}", props.next().unwrap()), "p_6");

        let mut props = PropSeq::new("prop");
        assert_eq!(format!("{}", props.next().unwrap()), "prop_0");
        props.nth(6);
        assert_eq!(format!("{}", props.next().unwrap()), "prop_8");
    }
}
