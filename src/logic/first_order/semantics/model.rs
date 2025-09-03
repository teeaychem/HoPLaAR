use crate::logic::first_order::{Element, Relation, Valuation, domains::Domain, terms::Fun};

// A model specifies:
// - A domain
// - A method to interpret functions
// - A method to interpret relations
pub trait Model<E: Element> {
    fn domain(&self) -> &Domain<E>;

    fn elements(&self) -> &[E];

    fn functions(&self, f: &Fun, v: &Valuation<E>) -> E;

    fn relations(&self, r: &Relation, v: &Valuation<E>) -> bool;
}
