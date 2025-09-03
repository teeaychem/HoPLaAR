use std::collections::HashMap;

use crate::logic::first_order::{Element, terms::Var};

#[derive(Debug, Clone, Default)]
pub struct Valuation<E: Element> {
    map: HashMap<Var, E>,
}

impl<E: Element> Valuation<E> {
    pub fn from<const N: usize>(arr: [(&str, E); N]) -> Valuation<E> {
        let mut v = Self {
            map: HashMap::with_capacity(arr.len()),
        };
        for (v_id, e) in arr {
            v.insert_id(v_id, e);
        }

        v
    }
}

impl<E: Element> Valuation<E> {
    pub fn get(&self, v: &Var) -> Option<&E> {
        self.map.get(v)
    }

    pub fn get_id(&self, v_id: &str) -> Option<&E> {
        self.map.get(&Var::from(v_id))
    }
}

impl<E: Element> Valuation<E> {
    pub fn insert(&mut self, v: Var, e: E) -> Option<E> {
        self.map.insert(v, e)
    }

    pub fn insert_id(&mut self, v_id: &str, e: E) -> Option<E> {
        self.map.insert(Var::from(v_id), e)
    }

    pub fn remove(&mut self, v: &Var) -> Option<E> {
        self.map.remove(v)
    }

    pub fn remove_id(&mut self, v_id: &str) -> Option<E> {
        self.map.remove(&Var::from(v_id))
    }
}
