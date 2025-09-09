use crate::logic::{Atomic, Literal, formula_set::FormulaSet};

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
}
