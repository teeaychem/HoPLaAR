use crate::logic::first_order::{
    Relation, Valuation, domains::Domain, semantics::model::Model, terms::Fun,
};

impl Domain<bool> {
    pub fn boolean() -> Self {
        Domain::from([true, false].into_iter())
    }
}

impl Valuation<bool> {
    fn boolean() -> Self {
        Self::undefined()
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
        match (f.id.as_str(), f.args.as_slice()) {
            ("0", []) => false,
            ("1", []) => true,
            ("add", [a, b]) => a.eval(self, v) != b.eval(self, v),
            ("mul", [a, b]) => a.eval(self, v) && b.eval(self, v),

            _ => todo!("Request to interpret function: {}", f.id),
        }
    }

    fn relations(&self, r: &Relation, v: &Valuation<bool>) -> bool {
        match (r.id.as_str(), r.terms.as_slice()) {
            ("eq", [a, b]) => a.eval(self, v) == b.eval(self, v),
            ("is_true", [a]) => a.eval(self, v),
            ("is_false", [a]) => !a.eval(self, v),

            _ => todo!("Request to interpret relation: {}", r.id),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::first_order::{FirstOrderFormula, Valuation, domains::Domain};

    #[test]
    #[allow(non_snake_case)]
    fn boolean() {
        let M = Domain::boolean();
        let mut v = Valuation::boolean();

        let expr = FirstOrderFormula::from("forall x. (eq(x, 0) | eq(x,1))");
        assert!(expr.eval(&M, &mut v));

        let expr = FirstOrderFormula::from("forall a. exists b. ~(is_true(mul(a,b)))");
        assert!(expr.eval(&M, &mut v));

        // As disjunction is exclusive
        let expr = FirstOrderFormula::from("exists b. ~(is_true(add(1,b)))");
        assert!(expr.eval(&M, &mut v));

        let expr = FirstOrderFormula::from("forall b. ~(is_true(add(1,b)))");
        assert!(!expr.eval(&M, &mut v));
    }
}
