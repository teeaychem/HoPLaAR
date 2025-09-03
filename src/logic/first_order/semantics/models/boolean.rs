use crate::logic::first_order::{
    InterpretationF, Model, Relation, Valuation, domains::Domain, terms::Fun,
};

fn interpret_bool_function(fun: &Fun, v: &Valuation<bool>) -> bool {
    match (fun.id(), fun.args()) {
        ("0", []) => false,
        ("1", []) => true,
        ("add", [x, y]) => {
            let x_val = x.eval(interpret_bool_function, v);
            let y_val = y.eval(interpret_bool_function, v);
            x_val != y_val
        }
        ("mul", [x, y]) => {
            let x_val = x.eval(interpret_bool_function, v);
            let y_val = y.eval(interpret_bool_function, v);
            x_val && y_val
        }

        _ => todo!("Request for term: {}", fun.id()),
    }
}

#[allow(non_snake_case)]
fn interpret_bool_relation(rel: &Relation, I: InterpretationF<bool>, v: &Valuation<bool>) -> bool {
    match (rel.id(), rel.terms()) {
        ("eq", [x, y]) => {
            let x_val = x.eval(I, v);
            let y_val = y.eval(I, v);
            x_val == y_val
        }
        ("is_true", [x]) => x.eval(I, v),

        _ => todo!("Request for relation: {}", rel.id()),
    }
}

impl Model<bool> {
    fn boolean() -> Self {
        Model::from(
            Domain::from(&[true, false]),
            interpret_bool_function,
            interpret_bool_relation,
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::logic::first_order::{Model, parse, terms::Var};

    #[test]
    fn valuation_basic() {
        let m_boolean = Model::boolean();

        let mut v = HashMap::from([(Var::from("x"), true), (Var::from("y"), false)]);
        let _ = v.insert(Var::from("y"), true);

        let expr = parse("exists a is_true(a)");
        let result = expr.eval(&m_boolean, &mut v);
        println!("{expr} {result}");

        let expr = parse("forall a. exists b. ~(is_true(mul(a,b)))");
        let result = expr.eval(&m_boolean, &mut v);
        println!("{expr} {result}");
    }
}
