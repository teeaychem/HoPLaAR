use crate::logic::first_order::{
    Element, Relation, Valuation, domains::Domain, semantics::model::Model, terms::Fun,
};

pub struct Empty<E> {
    domain: Domain<E>,
}

impl<E: Element> Model<E> for Empty<E> {
    fn domain(&self) -> &Domain<E> {
        &self.domain
    }

    fn elements(&self) -> &[E] {
        self.domain.elements()
    }

    fn functions(&self, f: &Fun, _: &Valuation<E>) -> E {
        todo!("Request to interpret function: {}", f.id)
    }

    fn relations(&self, r: &Relation, _: &Valuation<E>) -> bool {
        todo!("Request to interpret relation: {}", r.id)
    }
}
