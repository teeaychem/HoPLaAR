use hoplaar::logic::parse_propositional_formula;

fn main() {
    let expr = parse_propositional_formula("p /\\ q ==> q /\\ r");
    expr.print_truth_table();
    println!();

    let expr = parse_propositional_formula("((p ==> q) ==> p) ==> p");
    expr.print_truth_table();
    println!();

    let expr = parse_propositional_formula("p /\\ ~p");
    expr.print_truth_table();
}
