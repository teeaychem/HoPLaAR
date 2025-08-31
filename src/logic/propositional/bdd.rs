use std::collections::HashMap;

use crate::logic::{
    Formula, OpBinary, OpUnary,
    propositional::{Prop, PropFormula},
};

pub type BDDIndex = i32;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct BDDNode {
    prop: Prop,
    tb: BDDIndex,
    fb: BDDIndex,
}

impl std::fmt::Display for BDDNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({}, {})", self.prop, self.tb, self.fb)
    }
}

impl BDDNode {
    pub fn from(prop: Prop, tb: BDDIndex, fb: BDDIndex) -> Self {
        Self { prop, tb, fb }
    }

    pub fn invert(&mut self) {
        self.tb = -self.tb;
        self.fb = -self.fb;
    }
}

#[derive(Debug)]
struct BDDIndicies {
    idx: BDDIndex,
}

impl Iterator for BDDIndicies {
    type Item = BDDIndex;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < BDDIndex::MAX {
            let this = self.idx;
            self.idx += 1;
            Some(this)
        } else {
            None
        }
    }
}

impl Default for BDDIndicies {
    fn default() -> Self {
        Self { idx: 2 }
    }
}

#[derive(Debug, Default)]
pub struct BDDGraph {
    indicies: BDDIndicies,
    by_index: HashMap<BDDIndex, BDDNode>,
    by_nodes: HashMap<BDDNode, BDDIndex>,
    compilations: HashMap<(BDDIndex, BDDIndex), BDDIndex>,
}

impl std::fmt::Display for BDDGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, node) in &self.by_index {
            writeln!(f, "{idx} : {node}")?;
        }
        Ok(())
    }
}

impl BDDGraph {
    pub fn insert(&mut self, node: BDDNode) -> BDDIndex {
        let idx = self.indicies.next().expect("…");
        self.by_index.insert(idx, node.clone());
        self.by_nodes.insert(node, idx);
        idx
    }

    pub fn expand_node(&self, idx: &BDDIndex) -> Option<BDDNode> {
        match idx.is_positive() {
            true => self.by_index.get(&idx.abs()).cloned(),
            false => {
                let mut node = self.by_index.get(&idx.abs()).cloned()?;
                node.invert();
                Some(node)
            }
        }
    }

    fn bdd_make_node(&mut self, prop: Prop, l: BDDIndex, r: BDDIndex) -> BDDIndex {
        if l == r {
            1
        } else if l.is_positive() {
            let node = BDDNode::from(prop, l, r);

            match self.by_nodes.get(&node) {
                Some(idx) => *idx,
                None => self.insert(node),
            }
        } else {
            let node = BDDNode::from(prop, -l, -r);

            match self.by_nodes.get(&node) {
                Some(idx) => *idx,
                None => self.insert(node),
            }
        }
    }

    fn bdd_make_and(&mut self, lhs_idx: BDDIndex, rhs_idx: BDDIndex) -> BDDIndex {
        match lhs_idx {
            -1 => return -1,
            1 => return rhs_idx,
            _ => {}
        }

        match rhs_idx {
            -1 => return -1,
            1 => return lhs_idx,
            _ => {}
        }

        if let Some(idx) = self.compilations.get(&(lhs_idx, rhs_idx)) {
            return *idx;
        }
        if let Some(idx) = self.compilations.get(&(rhs_idx, lhs_idx)) {
            return *idx;
        }

        let lhs = self.expand_node(&lhs_idx).expect("!");
        let rhs = self.expand_node(&rhs_idx).expect("!");

        let (prop, (lt, lf), (rt, rf)) = {
            use std::cmp::Ordering::*;

            match lhs.prop.cmp(&rhs.prop) {
                Equal => (lhs.prop, (lhs.tb, lhs.fb), (rhs.tb, rhs.fb)),
                Less => (lhs.prop, (lhs.tb, rhs_idx), (lhs.fb, rhs_idx)),
                Greater => (rhs.prop, (lhs_idx, rhs.tb), (lhs_idx, rhs.fb)),
            }
        };

        let l_new = self.bdd_make_and(lt, lf);
        let r_new = self.bdd_make_and(rt, rf);

        let idx = self.bdd_make_node(prop, l_new, r_new);

        self.compilations.insert((lhs_idx, rhs_idx), idx);

        idx
    }

    pub fn mkbdd(&mut self, formula: &PropFormula) -> BDDIndex {
        match formula {
            Formula::True => 1,
            Formula::False => -1,

            Formula::Atom { var } => self.bdd_make_node(var.clone(), 1, -1),

            Formula::Unary { op, expr } => match op {
                OpUnary::Not => -self.mkbdd(expr),
            },

            Formula::Binary { op, lhs, rhs } => {
                let lhs_idx = self.mkbdd(lhs);
                let rhs_idx = self.mkbdd(rhs);

                match op {
                    OpBinary::And => self.bdd_make_and(lhs_idx, rhs_idx),
                    OpBinary::Or => -self.bdd_make_and(-lhs_idx, -rhs_idx),
                    OpBinary::Imp => -self.bdd_make_and(-lhs_idx, rhs_idx),
                    OpBinary::Iff => {
                        let ltr = -self.bdd_make_and(-lhs_idx, rhs_idx);
                        let rtl = -self.bdd_make_and(-rhs_idx, lhs_idx);
                        self.bdd_make_and(ltr, rtl)
                    }
                }
            }

            Formula::Quantifier { .. } => todo!(),
        }
    }
}

impl PropFormula {
    pub fn bdd(self) {
        let mut graph = BDDGraph::default();
        let head = graph.mkbdd(&self);
        println!("Head: {head}");
        println!("{graph}");
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::parse_propositional_formula;

    #[test]
    fn basic() {
        let expr = parse_propositional_formula("p & q & r & s");
        expr.bdd();

        let expr = parse_propositional_formula("-p & -q");
        expr.bdd();

        let expr = parse_propositional_formula("p | q");
        expr.bdd();
    }
}
