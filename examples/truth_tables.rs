use hoplaar::logic::parse_propositional_formula;

fn main() {
    let expr = parse_propositional_formula("p /\\ q ==> q /\\ r");
    println!("{expr}");
    println!("{}", expr.truth_table());

    let expr = parse_propositional_formula("((p ==> q) ==> p) ==> p");
    println!("{expr}");
    println!("{}", expr.truth_table());

    let expr = parse_propositional_formula("p /\\ ~p");
    println!("{expr}");
    println!("{}", expr.truth_table());
}
