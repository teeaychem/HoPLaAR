use std::{collections::HashMap, iter::Peekable};

use crate::logic::propositional::{Prop, PropFormula, prop::PropSeq};

#[derive(Debug)]
pub struct PropDict {
    defs: Peekable<PropSeq>,
    map: HashMap<PropFormula, Prop>,
}

impl Default for PropDict {
    fn default() -> Self {
        Self {
            defs: PropSeq::default().peekable(),
            map: Default::default(),
        }
    }
}

impl PropDict {
    pub fn map(&self) -> &HashMap<PropFormula, Prop> {
        &self.map
    }
}

impl PropDict {
    pub fn get_or_insert(&mut self, formula: PropFormula) -> Prop {
        match self.map.get(&formula) {
            Some(def) => def.clone(),

            None => {
                let atoms = formula.atoms();
                while self.defs.peek().is_some_and(|def| atoms.contains(def)) {
                    self.defs.next();
                }

                let def = match self.defs.peek() {
                    Some(def) => {
                        self.map.insert(formula, def.clone());
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

impl PropFormula {
    // pub fn cnf()
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
        let def = dict.get_or_insert(expr);
        assert_eq!(def, Prop::from("p_1"));

        let expr = parse_propositional_formula("p & q");
        let def = dict.get_or_insert(expr);
        assert_eq!(def, Prop::from("p_2"));
    }
}
