use std::{collections::HashMap, iter::Peekable};

use crate::logic::{
    Formula, OpBinary,
    propositional::{Prop, PropFormula, prop::PropSeq},
};

// A mapping from formulas to definition atoms.
//
// Definition atoms are generated from `defs`.
// `get_or_insert` checks no definition atom appears in the defined formula.
#[derive(Debug)]
pub struct PropDict {
    defs: Peekable<PropSeq>,
    definitions: HashMap<PropFormula, Prop>,
}

impl Default for PropDict {
    fn default() -> Self {
        Self {
            defs: PropSeq::default().peekable(),
            definitions: Default::default(),
        }
    }
}

impl PropDict {
    // The definitions, as a map from formulas to defining props.
    pub fn definitions(&self) -> &HashMap<PropFormula, Prop> {
        &self.definitions
    }
}

impl PropDict {
    // Updates `self` with a definition for `formula` if needed, then returns the defining prop for `formula`.
    pub fn get_or_insert(&mut self, formula: &PropFormula) -> Prop {
        match self.definitions.get(formula) {
            Some(def) => def.clone(),

            None => {
                let atoms = formula.atoms();
                while self.defs.peek().is_some_and(|def| atoms.contains(def)) {
                    self.defs.next();
                }

                let def = match self.defs.peek() {
                    Some(def) => {
                        self.definitions.insert(formula.clone(), def.clone());
                        def.clone()
                    }
                    None => panic!(),
                };

                self.defs.next();
                def
            }
        }
    }
}

impl PropDict {
    // Generates a `formula` iff `def` in CNF form, if `formula` is in suitable form.
    // I.e. (¬formula ∨ def) ∧ (¬def ∨ formula)
    // `formula` being in suitable form means:
    // - It contains at most one binary connective.
    // - Unary connectives only occur in literals.
    // - It does not contain any stacked unary connectives.
    pub fn def_to_three_cnf(formula: &PropFormula, def: &Prop) -> PropFormula {
        use {Formula::*, OpBinary::*};

        let def = PropFormula::Atom(def.clone());

        match formula {
            True | False | Atom { .. } | Unary { .. } => panic!("Unexpected definition"),

            Quantifier { .. } => panic!("Definition of quantifier"),

            Binary { op, lhs, rhs } => match op {
                And => {
                    let a = Formula::Or(
                        Formula::Or(lhs.clone().negate(), rhs.clone().negate()),
                        def.clone(),
                    );
                    let b = Formula::Or(def.clone().negate(), *lhs.clone());
                    let c = Formula::Or(def.clone().negate(), *rhs.clone());

                    Formula::conjoin([a, b, c].into_iter())
                }

                Or => {
                    let a = Formula::Or(
                        def.clone().negate(),
                        Formula::Or(*lhs.clone(), *rhs.clone()),
                    );
                    let b = Formula::Or(lhs.clone().negate(), def.clone());
                    let c = Formula::Or(rhs.clone().negate(), def.clone());

                    Formula::conjoin([a, b, c].into_iter())
                }

                Imp => {
                    let a = Formula::Or(
                        def.clone().negate(),
                        Formula::Or(lhs.clone().negate(), *rhs.clone()),
                    );
                    let b = Formula::Or(*lhs.clone(), def.clone());
                    let c = Formula::Or(rhs.clone().negate(), def.clone());

                    Formula::conjoin([a, b, c].into_iter())
                }

                Iff => {
                    let l = Formula::conjoin(
                        [
                            Formula::Or(
                                def.clone().negate(),
                                Formula::Or(lhs.clone().negate(), *rhs.clone()),
                            ),
                            Formula::Or(*lhs.clone(), def.clone()),
                            Formula::Or(rhs.clone().negate(), def.clone()),
                        ]
                        .into_iter(),
                    );

                    let r = Formula::conjoin(
                        [
                            Formula::Or(
                                def.clone().negate(),
                                Formula::Or(rhs.clone().negate(), *lhs.clone()),
                            ),
                            Formula::Or(*rhs.clone(), def.clone()),
                            Formula::Or(lhs.clone().negate(), def.clone()),
                        ]
                        .into_iter(),
                    );

                    Formula::And(l, r)
                }
            },
        }
    }

    // Generates a conjunction of all definitions in CNF.
    // Note, the 'head' of any defined formula is not a conjunct.
    pub fn to_three_cnf(&self) -> PropFormula {
        let def_cnfs = self
            .definitions
            .iter()
            .map(|(fm, def)| PropDict::def_to_three_cnf(fm, def));
        Formula::conjoin(def_cnfs)
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{
        parse_propositional_formula,
        propositional::{Prop, PropDict},
    };

    #[test]
    fn dict() {
        let mut dict = PropDict::default();

        let expr = parse_propositional_formula("p_0");
        let def = dict.get_or_insert(&expr);
        assert_eq!(def, Prop::from("p_1"));

        let expr = parse_propositional_formula("p & q");
        let def = dict.get_or_insert(&expr);
        assert_eq!(def, Prop::from("p_2"));
    }
}
