use crate::logic::first_order::{
    InterpretationF, Model, Relation, Valuation, domains::Domain, terms::Fun,
};

fn functions(fun: &Fun, v: &Valuation<bool>) -> bool {
    match (fun.id(), fun.args()) {
        ("0", []) => false,
        ("1", []) => true,
        ("add", [a, b]) => a.eval(functions, v) != b.eval(functions, v),
        ("mul", [a, b]) => a.eval(functions, v) && b.eval(functions, v),

        _ => todo!("Request to interpret function: {}", fun.id()),
    }
}

#[allow(non_snake_case)]
fn relations(rel: &Relation, I: InterpretationF<bool>, v: &Valuation<bool>) -> bool {
    match (rel.id(), rel.terms()) {
        ("eq", [a, b]) => a.eval(I, v) == b.eval(I, v),
        ("is_true", [a]) => a.eval(I, v),
        ("is_false", [a]) => !a.eval(I, v),

        _ => todo!("Request to interpret relation: {}", rel.id()),
    }
}

impl Model<bool> {
    fn boolean() -> Self {
        Model::from(Domain::from(&[true, false]), functions, relations)
    }
}

impl Valuation<bool> {
    fn boolean() -> Self {
        Self::default()
    }
}

#[cfg(test)]
mod tests {

    use crate::logic::first_order::{Model, Valuation, parse};

    #[test]
    #[allow(non_snake_case)]
    fn boolean() {
        let M = Model::boolean();
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
