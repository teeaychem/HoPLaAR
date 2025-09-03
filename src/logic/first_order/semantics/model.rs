use crate::logic::first_order::{Element, Relation, Valuation, domains::Domain, terms::Fun};

pub type InterpretationF<D> = fn(&Fun, &Valuation<D>) -> D;

pub type InterpretationR<D> = fn(&Relation, InterpretationF<D>, &Valuation<D>) -> bool;

#[derive(Clone)]
pub struct Model<E: Element> {
    domain: Domain<E>,
    functions: InterpretationF<E>,
    relations: InterpretationR<E>,
}

impl<E: Element> Model<E> {
    pub fn from(
        domain: Domain<E>,
        functions: InterpretationF<E>,
        relations: InterpretationR<E>,
    ) -> Self {
        Self {
            domain,
            functions,
            relations,
        }
    }
}

impl<E: Element> Model<E> {
    pub fn domain(&self) -> &Domain<E> {
        &self.domain
    }

    pub fn elements(&self) -> &[E] {
        self.domain.elements()
    }

    pub fn interpret_function(&self, fun: &Fun, v: &Valuation<E>) -> E {
        (self.functions)(fun, v)
    }

    pub fn interpret_relation(&self, relation: &Relation, v: &Valuation<E>) -> bool {
        (self.relations)(relation, self.functions, v)
    }
}
