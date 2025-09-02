pub trait Element: std::fmt::Debug + std::fmt::Display + Clone {}

impl Element for bool {}

#[derive(Clone, Debug, Default)]
pub struct Domain<E> {
    elements: Vec<E>,
}

impl<E: Element> Domain<E> {
    pub fn from<const N: usize>(elements: &[E; N]) -> Self {
        Self {
            elements: elements.to_vec(),
        }
    }

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
