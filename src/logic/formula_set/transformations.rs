use std::collections::{HashMap, HashSet};

use crate::logic::{
    Atomic, Literal,
    formula_set::{FormulaSet, Mode},
};

impl<A: Atomic> FormulaSet<A> {
    /// Applies the affirmative / negative rule to `self`.
    /// Returns true if a mutation occurred, and false otherwise.
    pub fn one_literal_rule(&mut self) -> bool {
        // A collection of all one literal set literals
        let mut one_literals: Vec<Literal<A>> = Vec::default();

        let mut removed_other: HashSet<Literal<A>> = HashSet::default();

        let mut mutation = false;

        // Swap remove all one literal sets, exending one_literals with each set found.
        let mut index = 0;
        let mut limit = self.sets.len();

        while index < limit {
            match self.sets[index].len() {
                1 => {
                    let one_literal_set = self.sets.swap_remove(index);
                    one_literals.extend(one_literal_set);
                    mutation = true;
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
                                removed_other.extend(self.sets.swap_remove(set_index));
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

        // It's likely there are fewer one literals than atoms.
        // So, remove one literals by inspecting each atom.
        // Here, atoms could be sorted and binary search could be used.
        for literal in one_literals {
            self.atoms.remove(literal.id());
        }

        'other_loop: for other in removed_other {
            if self.sets.iter().flatten().any(|l| l == &other) {
                continue 'other_loop;
            } else if let Some((t, f)) = self.atoms.get_mut(other.id()) {
                match other.value() {
                    true => *t = false,
                    false => *f = false,
                }
            }
        }

        // TODO: Atom may now contain false / false values.

        mutation
    }

    /// Applies the affirmative / negative rule to `self`.
    /// Returns true if a mutation occurred, and false otherwise.
    pub fn affirmative_negative_rule(&mut self) -> bool {
        // Map each atom id to a pair, capturing whether the atom has appear in a (true, false) literal.
        // Retain only those instance when the atom has not appear both as true and false.
        let atom_ids: Vec<String> = self
            .atoms
            .iter()
            .filter_map(|(id, (t, f))| match !(*t && *f) {
                true => Some(id.to_owned()),
                false => None,
            })
            .collect();

        let mut mutation = false;

        let mut set_index = 0;
        let mut set_limit = self.sets.len();

        'set_loop: while set_index < set_limit {
            for literal in &self.sets[set_index] {
                for atom in &atom_ids {
                    if *atom == literal.id() {
                        set_limit -= 1;
                        self.sets.swap_remove(set_index);
                        mutation = true;
                        continue 'set_loop;
                    }
                }
            }
            set_index += 1;
        }

        // Remove from atoms any affirmative / negative atoms.
        for id in atom_ids {
            self.atoms.remove(&id);
        }

        mutation
    }

    /// Applies the affirmative / negative rule to `self`, using `atom` as a pivot.
    /// Nominally returns true if a mutation occurred, and false otherwise.
    /// Though, at present panics if `atom` occurrs only positively or negatively.
    pub fn resolve_on(&mut self, id: &str) -> bool {
        // Each set of literals containing atom / ~atom will be moved to local storage.
        // Further, one move atom / ~atom will be removed from the set to ease taking the product later.
        let mut positive = FormulaSet::<A>::empty(Mode::CNF);
        let mut negative = FormulaSet::<A>::empty(Mode::CNF);

        let mut set_index = 0;
        let mut set_limit = self.sets.len();

        'set_loop: while set_index < set_limit {
            let mut literal_index = 0;
            let literal_limit = self.sets[set_index].len();

            while literal_index < literal_limit {
                if self.sets[set_index][literal_index].id() == id {
                    let literal = self.sets[set_index].swap_remove(literal_index);

                    match literal.value() {
                        true => positive.sets.push(self.sets.swap_remove(set_index)),
                        false => negative.sets.push(self.sets.swap_remove(set_index)),
                    }
                    set_limit -= 1;
                    continue 'set_loop;
                }

                literal_index += 1;
            }
            set_index += 1;
        }

        if positive.is_top() || negative.is_top() {
            panic!("Resolution called on {id} without complimentary literals")
        }

        // Take the cartersian product
        for a in &positive.sets {
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

        // Remove the atom used as a pivot.
        self.atoms.remove(id);

        true
    }

    /// The relative size of `self` after applying `resolve_on` with `atom`.
    pub fn resolution_blowup(&self, id: &str) -> isize {
        // Note, an isize is returned as the formula *may* shrink.

        let mut positive_count: isize = 0;
        let mut negative_count: isize = 0;

        for set in &self.sets {
            for literal in set {
                if literal.id() == id {
                    match literal.value() {
                        true => positive_count += 1,
                        false => negative_count += 1,
                    }
                }
            }
        }

        (positive_count * negative_count) - (positive_count + negative_count)
    }

    pub fn resolution_rule(&mut self) -> bool {
        let min_atom = match self
            .atoms
            .iter()
            .filter_map(|(id, (t, f))| match *t && *f {
                true => Some(id),
                false => None,
            })
            .min_by(|a, b| self.resolution_blowup(a).cmp(&self.resolution_blowup(b)))
        {
            Some(a) => a.clone(),
            None => return false,
        };

        println!("{min_atom}");

        self.resolve_on(&min_atom)
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Atomic, parse_propositional, propositional::Prop};

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

        let prop = Prop::from("p");

        println!("{}", fm.resolution_blowup(prop.id()));

        fm.resolution_rule();

        println!("{fm}");
    }
}
