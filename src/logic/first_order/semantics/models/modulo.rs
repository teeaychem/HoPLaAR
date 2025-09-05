use crate::logic::first_order::{
    Relation, Valuation, domains::Domain, semantics::model::Model, terms::Fun,
};

pub struct Modulo {
    n: usize,
    domain: Domain<usize>,
}

impl Modulo {
    pub fn n(n: usize) -> Self {
        Self {
            n,
            domain: Domain::from(0..n),
        }
    }
}

impl Valuation<usize> {
    fn modulo() -> Self {
        Self::undefined()
    }
}

impl Model<usize> for Modulo {
    fn domain(&self) -> &Domain<usize> {
        &self.domain
    }

    fn elements(&self) -> &[usize] {
        self.domain.elements()
    }

    fn functions(&self, f: &Fun, v: &Valuation<usize>) -> usize {
        match (f.id.as_str(), f.args.as_slice()) {
            ("0", []) => 0,
            ("1", []) => 1 % self.n,
            ("add", [a, b]) => (a.eval(self, v) + b.eval(self, v)) % self.n,
            ("mul", [a, b]) => (a.eval(self, v) * b.eval(self, v)) % self.n,

            _ => todo!("Request to interpret function: {}", f.id),
        }
    }

    fn relations(&self, r: &Relation, v: &Valuation<usize>) -> bool {
        match (r.id.as_str(), &r.terms.as_slice()) {
            ("eq", [a, b]) => a.eval(self, v) == b.eval(self, v),

            _ => todo!("Request to interpret relation: {}", r.id),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::first_order::{
        FirstOrderFormula, Valuation, semantics::models::modulo::Modulo,
    };

    #[test]
    fn simple() {
        let mut v = Valuation::modulo();

        let expr = FirstOrderFormula::from("forall x. (eq(x, 0) | eq(x,1))");
        assert!(expr.eval(&Modulo::n(2), &mut v));

        let expr = FirstOrderFormula::from("forall x. (eq(x, 0) | eq(x,1))");
        assert!(!expr.eval(&Modulo::n(3), &mut v));
    }

    #[test]
    fn inverse() {
        let mut v = Valuation::modulo();

        let expr = FirstOrderFormula::from("forall x. (~eq(x, 0) => exists y. eq(mul(x,y), 1)))");

        let ok: Vec<usize> = (1_usize..45)
            .filter(|&n| expr.eval(&Modulo::n(n), &mut v))
            .collect();

        assert_eq!(ok, [1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43]);
    }
}
