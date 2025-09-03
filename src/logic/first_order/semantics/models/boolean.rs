use crate::logic::first_order::{
    Relation, Valuation, domains::Domain, semantics::model::Model, terms::Fun,
};

impl Domain<bool> {
    fn boolean() -> Self {
        Domain::from([true, false].into_iter())
    }
}

impl Valuation<bool> {
    fn boolean() -> Self {
        Self::default()
    }
}

impl Model<bool> for Domain<bool> {
    fn domain(&self) -> &Domain<bool> {
        self
    }

    fn elements(&self) -> &[bool] {
        self.elements()
    }

    fn functions(&self, f: &Fun, v: &Valuation<bool>) -> bool {
        match (f.id(), f.args()) {
            ("0", []) => false,
            ("1", []) => true,
            ("add", [a, b]) => a.eval(self, v) != b.eval(self, v),
            ("mul", [a, b]) => a.eval(self, v) && b.eval(self, v),

            _ => todo!("Request to interpret function: {}", f.id()),
        }
    }

    fn relations(&self, r: &Relation, v: &Valuation<bool>) -> bool {
        match (r.id(), r.terms()) {
            ("eq", [a, b]) => a.eval(self, v) == b.eval(self, v),
            ("is_true", [a]) => a.eval(self, v),
            ("is_false", [a]) => !a.eval(self, v),

            _ => todo!("Request to interpret relation: {}", r.id()),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::first_order::{Valuation, domains::Domain, parse};

    #[test]
    #[allow(non_snake_case)]
    fn boolean() {
        let M = Domain::boolean();
        let mut v = Valuation::boolean();

        let expr = parse("forall x. (eq(x, 0) | eq(x,1))");
        assert!(expr.eval(&M, &mut v));

        let expr = parse("forall a. exists b. ~(is_true(mul(a,b)))");
        assert!(expr.eval(&M, &mut v));

        // As disjunction is exclusive
        let expr = parse("exists b. ~(is_true(add(1,b)))");
        assert!(expr.eval(&M, &mut v));

        let expr = parse("forall b. ~(is_true(add(1,b)))");
        assert!(!expr.eval(&M, &mut v));
    }
}
