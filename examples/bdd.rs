use hoplaar::logic::parse_propositional_formula;

fn main() {
    let expr = parse_propositional_formula("p & q & r & s");
    let (head, graph) = expr.bdd();

    println!("\n{expr} -> {head}\n");
    for (idx, node) in graph.indies_to_nodes() {
        println!("{idx} : {node}");
    }
    println!("\n{}", graph.string_respresentation(head));

    let expr = parse_propositional_formula("-p & -q");
    let (head, graph) = expr.bdd();

    println!("\n{expr} -> {head}\n");
    for (idx, node) in graph.indies_to_nodes() {
        println!("{idx} : {node}");
    }
    println!("\n{}", graph.string_respresentation(head));

    let expr = parse_propositional_formula("p | q | long_r");
    let (head, graph) = expr.bdd();

    println!("\n{expr} -> {head}\n");
    for (idx, node) in graph.indies_to_nodes() {
        println!("{idx} : {node}");
    }

    let expr = parse_propositional_formula("big_p <=> (q => ~r)");
    let (head, graph) = expr.bdd();

    println!("\n{expr} -> {head}\n");
    for (idx, node) in graph.indies_to_nodes() {
        println!("{idx} : {node}");
    }
    println!("\n{}", graph.string_respresentation(head));
}
