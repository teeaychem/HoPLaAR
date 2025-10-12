use std::collections::HashSet;

use crate::logic::{
    Atomic, Literal,
    formula_set::{FormulaSet, LiteralSet, Mode, OccurrenceMap},
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
                        self.add_set(LiteralSet::default());
                        return true;
                    }
                }
            }
        }

        'other_loop: for other in removed_other {
            if self
                .sets
                .iter()
                .flat_map(|set| set.atoms())
                .any(|l| l == &other.atom)
            {
                continue 'other_loop;
            }
        }

        true
    }

    /// Applies the affirmative / negative rule to `self`.
    /// Returns true if a mutation occurred, and false otherwise.
    pub fn affirmative_negative_rule(&mut self) -> bool {
        // Map each atom id to a pair, capturing whether the atom has appear in a (true, false) literal.
        // Retain only those instance when the atom has not appear both as true and false.
        let atom_ids: Vec<A> = self
            .occurrence_map()
            .into_iter()
            .filter_map(|(id, (n, p))| match n == 0 || p == 0 {
                true => Some(id),
                false => None,
            })
            .collect();

        let mut mutation = false;

        let mut set_index = 0;
        let mut set_limit = self.sets.len();
        let mut remove = false;

        while set_index < set_limit {
            'literal_loop: for literal in self.sets[set_index].atoms() {
                for atom in &atom_ids {
                    if atom == literal {
                        set_limit -= 1;
                        remove = true;
                        break 'literal_loop;
                    }
                }
            }
            match remove {
                true => {
                    self.sets.swap_remove(set_index);
                    remove = false;
                    mutation = true;
                }
                false => set_index += 1,
            }
        }

        mutation
    }

    /// Applies the affirmative / negative rule to `self`, using `atom` as a pivot.
    /// Nominally returns true if a mutation occurred, and false otherwise.
    /// Though, at present panics if `atom` occurrs only positively or negatively.
    pub fn resolve_on(&mut self, atom: &A) -> bool {
        // Each set of literals containing atom / ~atom will be moved to local storage.
        // Further, one move atom / ~atom will be removed from the set to ease taking the product later.
        let mut positive = FormulaSet::<A>::empty(Mode::CNF);
        let mut negative = FormulaSet::<A>::empty(Mode::CNF);

        let mut set_index = 0;
        let mut set_limit = self.sets.len();

        'set_loop: while set_index < set_limit {
            if let Some(literal) = self.sets[set_index].remove_atom(atom) {
                match literal.value {
                    true => positive.add_set(self.sets.swap_remove(set_index)),
                    false => negative.add_set(self.sets.swap_remove(set_index)),
                }
                set_limit -= 1;
                continue 'set_loop;
            }

            set_index += 1;
        }

        if positive.is_top() || negative.is_top() {
            todo!("Resolution called on {atom} without complimentary literals")
        }

        // Take the cartersian product
        for n in &negative.sets {
            for p in &positive.sets {
                let mut fresh_set: LiteralSet<A> = LiteralSet::default();
                fresh_set.extend(p.literals());
                fresh_set.extend(n.literals());

                // Skip tivial sets from resolution
                if !fresh_set.has_complementary_literals() {
                    // Extend the formula
                    self.add_set(fresh_set);
                }
            }
        }

        // Ensure the formula continues to emulate a set
        self.setify_outer();

        true
    }

    /// The relative size of `self` after applying `resolve_on` with `atom`.
    pub fn resolution_blowup(occurrence_map: &OccurrenceMap<A>, atom: &A) -> isize {
        // Note, an isize is returned as the formula *may* shrink.

        match occurrence_map.get(atom) {
            Some((n, p)) => {
                let n: isize = (*n).try_into().unwrap();
                let p: isize = (*p).try_into().unwrap();

                (p * n) - (p + n)
            }
            None => todo!(),
        }
    }

    pub fn resolution_rule(&mut self) -> bool {
        let occurrence_map = self.occurrence_map();

        let min_atom = match occurrence_map
            .iter()
            .filter_map(|(id, (n, p))| match *n > 0 && *p > 0 {
                true => Some(id),
                false => None,
            })
            .min_by(|a, b| {
                Self::resolution_blowup(&occurrence_map, a)
                    .cmp(&Self::resolution_blowup(&occurrence_map, b))
            }) {
            Some(atom) => atom,
            None => return false,
        };

        self.resolve_on(min_atom)
    }

    pub fn is_sat_dp(&mut self) -> bool {
        self.filter_trivial_cnf();
        loop {
            if self.is_bot() {
                return false;
            }

            if self.is_top() {
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
