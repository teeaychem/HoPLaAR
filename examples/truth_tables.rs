use hoplaar::logic::parse_propositional;

fn main() {
    let expr = parse_propositional("p /\\ q ==> q /\\ r");
    println!("{expr}");
    println!("{}", expr.truth_table());

    let expr = parse_propositional("((p ==> q) ==> p) ==> p");
    println!("{expr}");
    println!("{}", expr.truth_table());

    let expr = parse_propositional("p /\\ ~p");
    println!("{expr}");
    println!("{}", expr.truth_table());
}
