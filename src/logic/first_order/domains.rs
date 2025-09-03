pub trait Element: std::fmt::Debug + std::fmt::Display + Clone {}

impl Element for bool {}

impl Element for usize {}

#[derive(Clone, Debug, Default)]
pub struct Domain<E> {
    elements: Vec<E>,
}

impl<I: Iterator<Item = E>, E: Element> From<I> for Domain<E> {
    fn from(value: I) -> Self {
        let elements: Vec<E> = value.collect();

        if elements.is_empty() {
            panic!("Domains must be non-empty");
        }

        Self { elements }
    }
}

impl<E: Element> Domain<E> {
    pub fn elements(&self) -> &[E] {
        &self.elements
    }

    pub fn size(&self) -> usize {
        self.elements.len()
    }

    pub fn element(&self, index: usize) -> E {
        match self.elements.get(index) {
            Some(e) => e.clone(),
            None => todo!(),
        }
    }
}
