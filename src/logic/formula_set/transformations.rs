use std::collections::HashSet;

use crate::logic::{
    Atomic, Literal,
    formula_set::{FormulaSet, LiteralSet, Mode},
};

impl<A: Atomic> FormulaSet<A> {
    /// Applies the one literal rule to `self`.
    /// Returns true if a mutation occurred, and false otherwise.
    pub fn one_literal_rule(&mut self) -> bool {
        let mut removed_other: HashSet<Literal<A>> = HashSet::default();

        let mut one = None;

        for index in 0..self.sets.len() {
            if let 1 = self.sets[index].len() {
                let removed_set = self.sets.swap_remove(index);
                let one_literal = removed_set.into_literals().next().unwrap();
                one = Some(one_literal);
                break;
            }
        }

        let one = match one {
            Some(l) => l,
            None => return false,
        };

        // We now have all one literal literals in `one_literals`, so...
        // A second (final) pass over all sets.
        // As both sets and literals may be mutated, this is done as a nested swap remove.

        let mut set_index = 0;
        let mut set_limit = self.sets.len();

        'set_loop: while set_index < set_limit {
            // Check against each one literal.

            use super::literal_set::LiteralQuery;

            match self.sets[set_index].one_literal(&one, true) {
                LiteralQuery::Missing => {
                    // As the set was not removed, and all literals were examined, consider the next set.
                    set_index += 1;
                }
                LiteralQuery::Matching => {
                    set_limit -= 1;
                    let removed_set = self.sets.swap_remove(set_index);
                    removed_other.extend(removed_set.into_literals());
                    continue 'set_loop;
                }
                LiteralQuery::Conflicting => {
                    // If the literal was unit, have φ ∧ ¬φ, so rewrite to basic ⊥ form.
                    if self.sets[set_index].is_empty() {
                        self.sets.drain(0..set_limit);
                        self.sets.push(LiteralSet::default());
                        return true;
                    }
                }
            }
        }

        self.atoms.remove(one.id());

        'other_loop: for other in removed_other {
            if self
                .sets
                .iter()
                .flat_map(|set| set.literals())
                .any(|l| l == &other)
            {
                continue 'other_loop;
            } else if let Some((t, f)) = self.atoms.get_mut(other.id()) {
                match other.value() {
                    true => *t = false,
                    false => *f = false,
                }
            }
        }

        // TODO: Atom may now contain false / false values.

        true
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
            for literal in self.sets[set_index].literals() {
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
                if self.sets[set_index].atom_at(literal_index).id() == id {
                    let literal = self.sets[set_index].remove(literal_index);

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
        for p in &positive.sets {
            for n in &negative.sets {
                let mut fresh = p.clone();
                fresh.extend(n.literals().cloned());

                // Skip tivial sets from resolution
                if !fresh.has_complementary_literals() {
                    // Extend the formula
                    self.sets.push(fresh);
                }
            }
        }

        // Ensure the formula continues to emulate a set
        self.setify_outer();

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
            for literal in set.literals() {
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

    pub fn is_sat_dp(&mut self) -> bool {
        self.filter_trivial_cnf();
        loop {
            if self.is_bot() {
                return false;
            } else if self.is_top() {
                return true;
            }

            if self.one_literal_rule() {
                continue;
            }

            if self.affirmative_negative_rule() {
                continue;
            }

            self.resolution_rule();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{formula_set::Mode, parse_propositional};

    #[test]
    fn affirmative_negative_simple() {
        let expr = parse_propositional("(~p | r) & (q | p | r)");

        let mut cnf = expr.to_set_direct(Mode::CNF);

        cnf.affirmative_negative_rule();

        assert!(cnf.is_cnf_top());
    }

    #[test]
    fn dp_simple_bot() {
        let mut bot = parse_propositional("~p & p").to_cnf_formula_set_tseytin();
        assert!(!bot.is_sat_dp());
    }

    fn dp_simple_top() {
        let mut top = parse_propositional("~p | p").to_cnf_formula_set_tseytin();
        assert!(top.is_sat_dp());
    }

    #[test]
    fn dp_still_simple() {
        let mut fm =
            parse_propositional("(~p | r | s) & (q | p | r) & (s | t)").to_set_direct(Mode::CNF);
        println!("{}", fm.is_sat_dp());
    }
}
