/* Ramsey */

use hoplaar::logic::{
    Formula,
    propositional::{Prop, PropFormula},
};

pub fn allsets<T: Clone + Ord>(of_size: usize, vec: &Vec<T>) -> Vec<Vec<T>> {
    match of_size {
        0 => vec![vec![]],

        _ => {
            let one_smaller = allsets(of_size - 1, vec);

            vec.iter().fold(vec![], |mut acc, int| {
                let mut x = one_smaller
                    .clone()
                    .into_iter()
                    .filter(|subset| !subset.contains(int))
                    .collect::<Vec<Vec<T>>>();

                for subset in &mut x {
                    subset.push(int.clone());
                    subset.sort_unstable();

                    if !acc.contains(subset) {
                        acc.push(subset.to_vec())
                    }
                }
                acc
            })
        }
    }
}

fn pairs_to_props(pair: &[usize], negate: bool) -> Formula<Prop> {
    match pair {
        [a, b, ..] => {
            let prop = Prop::from(&format!("p_{a}_{b}"));
            let atom = PropFormula::Atom(prop);
            match negate {
                true => Formula::Not(atom),
                false => atom,
            }
        }
        _ => panic!(),
    }
}

#[allow(clippy::needless_late_init)]
pub fn ramsey(s: usize, t: usize, n: usize) -> PropFormula {
    let verticies: Vec<usize> = (1..n + 1).collect();

    let yes_groups = allsets(s, &verticies).into_iter().map(|v| allsets(2, &v));

    let yes_atoms: Vec<Vec<Formula<Prop>>>;
    yes_atoms = yes_groups
        .map(|v| v.iter().map(|pair| pairs_to_props(pair, false)).collect())
        .collect();

    let yes_conjuncts = yes_atoms
        .into_iter()
        .map(|v| Formula::conjoin(v.into_iter()));

    let yes_expression = Formula::disjoin(yes_conjuncts);

    let no_groups = allsets(t, &verticies).into_iter().map(|v| allsets(2, &v));

    let no_atoms: Vec<Vec<Formula<Prop>>>;
    no_atoms = no_groups
        .map(|v| v.iter().map(|pair| pairs_to_props(pair, true)).collect())
        .collect();

    let no_conjuncts = no_atoms
        .into_iter()
        .map(|v| Formula::conjoin(v.into_iter()));

    let no_expression = Formula::disjoin(no_conjuncts);

    PropFormula::Or(yes_expression, no_expression)
}

fn main() {
    let three_three_four = ramsey(3, 3, 4);
    println!("{three_three_four}");

    assert!(!ramsey(3, 3, 5).is_tautology());
    assert!(ramsey(3, 3, 6).is_tautology());
}
