use std::collections::HashMap;

use crate::logic::{
    Atomic, Literal,
    formula_set::{FormulaSet, Mode},
};

impl<A: Atomic> FormulaSet<A> {
    pub fn one_literal_rule(&mut self) {
        // A collection of all one literal set literals
        let mut one_literals: Vec<Literal<A>> = Vec::default();

        // Swap remove all one literal sets, exending one_literals with each set found.
        let mut index = 0;
        let mut limit = self.sets.len();

        while index < limit {
            match self.sets[index].len() {
                1 => {
                    let one_literal_set = self.sets.swap_remove(index);
                    one_literals.extend(one_literal_set);
                    limit -= 1
                }

                _ => index += 1,
            }
        }

        // We now have all one literal literals in `one_literals`, so...
        // A second (final) pass over all sets.
        // As both sets and literals may be mutated, this is done as a nested swap remove.

        let mut set_index = 0;
        let mut set_limit = self.sets.len();

        'set_loop: while set_index < set_limit {
            let mut literal_index = 0;
            let mut literal_limit = self.sets[set_index].len();

            'literal_loop: while literal_index < literal_limit {
                // Check against each one literal.
                for literal in &one_literals {
                    // If the atoms match, either the set or the literal will be removed.
                    if literal.atom() == self.sets[set_index][literal_index].atom() {
                        match literal
                            .value()
                            .cmp(&self.sets[set_index][literal_index].value())
                        {
                            std::cmp::Ordering::Equal => {
                                set_limit -= 1;
                                self.sets.swap_remove(set_index);
                                continue 'set_loop;
                            }

                            _ => {
                                literal_limit -= 1;
                                self.sets[set_index].swap_remove(literal_index);
                                continue 'literal_loop;
                            }
                        }
                    }
                }
                // As the set was not removed, consider the next literal.
                literal_index += 1;
            }
            // As the set was not removed, and all literals were examined, consider the next set.
            set_index += 1;
        }
    }

    pub fn affirmative_negative_rule(&mut self) {
        let atom_ids: Vec<String> = {
            // Map each atom id to a pair, capturing whether the atom has appear in a (true, false) literal.
            let mut instances: HashMap<&str, (bool, bool)> = HashMap::default();

            for literal in self.sets.iter().flatten() {
                match literal.value() {
                    true => instances.entry(literal.id()).or_default().0 = true,
                    false => instances.entry(literal.id()).or_default().1 = true,
                }
            }

            // Retain only those instance when the atom has not appear both as true and false.
            instances.retain(|_, (t, f)| !(*t && *f));

            // Collect the ids, as owned, given the formula set is up for mutation.
            instances.into_keys().map(|k| k.to_owned()).collect()
        };

        let mut set_index = 0;
        let mut set_limit = self.sets.len();

        'set_loop: while set_index < set_limit {
            for literal in &self.sets[set_index] {
                for atom in &atom_ids {
                    if *atom == literal.id() {
                        set_limit -= 1;
                        self.sets.swap_remove(set_index);
                        continue 'set_loop;
                    }
                }
            }
            set_index += 1;
        }
    }

    pub fn resolve_on(&mut self, atom: A) {
        // Each set of literals containing atom / ~atom will be moved to local storage.
        // Further, one move atom / ~atom will be removed from the set to ease taking the product later.
        let mut affirmative = FormulaSet::<A>::empty(Mode::CNF);
        let mut negative = FormulaSet::<A>::empty(Mode::CNF);

        let mut set_index = 0;
        let mut set_limit = self.sets.len();

        'set_loop: while set_index < set_limit {
            let mut literal_index = 0;
            let literal_limit = self.sets[set_index].len();

            while literal_index < literal_limit {
                if self.sets[set_index][literal_index].atom() == &atom {
                    let literal = self.sets[set_index].swap_remove(literal_index);

                    match literal.value() {
                        true => affirmative.sets.push(self.sets.swap_remove(set_index)),
                        false => negative.sets.push(self.sets.swap_remove(set_index)),
                    }
                    set_limit -= 1;
                    continue 'set_loop;
                }

                literal_index += 1;
            }
            set_index += 1;
        }

        // Take the cartersian product
        for a in &affirmative.sets {
            for n in &negative.sets {
                let mut fresh = a.clone();
                fresh.extend(n.iter().cloned());

                // Ensure the fresh vec emulates a set
                fresh.sort_unstable();
                fresh.dedup();

                // Extend the formula
                self.sets.push(fresh);
            }
        }

        // Ensure the formula continues to emulate a set
        self.sets.sort_unstable();
        self.sets.dedup();
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{parse_propositional, propositional::Prop};

    #[test]
    fn affirmative_negative_simple() {
        let expr = parse_propositional("(~p | r) & (q | p | r)");

        let mut cnf = expr.to_cnf_set_direct();

        cnf.affirmative_negative_rule();

        assert!(cnf.is_cnf_top());
    }

    #[test]
    fn debug() {
        let mut fm =
            parse_propositional("(~p | r | s) & (q | p | r) & (s | t)").to_cnf_set_direct();
        fm.resolve_on(Prop::from("p"));
    }
}
