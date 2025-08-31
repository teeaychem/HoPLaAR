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

    pub fn top() -> Self {
        Self {
            prop: Prop::from("t"),
            tb: 1,
            fb: 1,
        }
    }

    pub fn bot() -> Self {
        Self {
            prop: Prop::from("f"),
            tb: -1,
            fb: -1,
        }
    }

    pub fn invert(mut self) -> BDDNode {
        self.tb = -self.tb;
        self.fb = -self.fb;
        self
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

// 0 is unused with 1 reserved for true (and -1 for false)
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

    pub fn get_or_insert(&mut self, node: BDDNode) -> BDDIndex {
        match self.by_nodes.get(&node).cloned() {
            Some(idx) => idx,
            None => {
                let idx = self.indicies.next().expect("…");
                self.by_index.insert(idx, node.clone());
                self.by_nodes.insert(node, idx);
                idx
            }
        }
    }

    pub fn expand_node(&self, idx: &BDDIndex) -> BDDNode {
        match idx.is_positive() {
            true => match self.by_index.get(idx) {
                Some(node) => node.clone(),
                None => BDDNode::top(),
            },
            false => match self.by_index.get(&-idx) {
                Some(node) => node.clone().invert(),
                None => BDDNode::bot(),
            },
        }
    }

    fn bdd_make_node(&mut self, prop: Prop, t: BDDIndex, f: BDDIndex) -> BDDIndex {
        if t == f {
            1
        } else if t.is_positive() {
            let node = BDDNode::from(prop, t, f);
            self.get_or_insert(node)
        } else {
            let node = BDDNode::from(prop, -t, -f);
            -self.get_or_insert(node)
        }
    }

    fn bdd_make_and(&mut self, t_idx: BDDIndex, f_idx: BDDIndex) -> BDDIndex {
        match (t_idx, f_idx) {
            (-1, _) | (_, -1) => return -1,
            (1, _) => return f_idx,
            (_, 1) => return t_idx,
            _ => {}
        }

        if let Some(idx) = self.compilations.get(&(t_idx, f_idx)) {
            return *idx;
        }

        if let Some(idx) = self.compilations.get(&(f_idx, t_idx)) {
            return *idx;
        }

        let t = self.expand_node(&t_idx);
        let f = self.expand_node(&f_idx);

        let (prop, (lt, lf), (rt, rf)) = {
            use std::cmp::Ordering::*;

            match t.prop.cmp(&f.prop) {
                Equal => (t.prop, (t.tb, t.fb), (f.tb, f.fb)),
                Less => (t.prop, (t.tb, f_idx), (t.fb, f_idx)),
                Greater => (f.prop, (t_idx, f.tb), (t_idx, f.fb)),
            }
        };

        let t_new = self.bdd_make_and(lt, lf);
        let f_new = self.bdd_make_and(rt, rf);

        let idx = self.bdd_make_node(prop, t_new, f_new);

        self.compilations.insert((t_idx, f_idx), idx);

        idx
    }

    pub fn bdd_make(&mut self, formula: &PropFormula) -> BDDIndex {
        match formula {
            Formula::True => 1,
            Formula::False => -1,

            Formula::Atom { var } => self.bdd_make_node(var.clone(), 1, -1),

            Formula::Unary { op, expr } => {
                let expr_node = self.bdd_make(expr);

                match op {
                    OpUnary::Not => -expr_node,
                }
            }

            Formula::Binary { op, lhs, rhs } => {
                let lhs_idx = self.bdd_make(lhs);
                let rhs_idx = self.bdd_make(rhs);

                match op {
                    OpBinary::And => self.bdd_make_and(lhs_idx, rhs_idx),
                    OpBinary::Or => -self.bdd_make_and(-lhs_idx, -rhs_idx),
                    OpBinary::Imp => -self.bdd_make_and(lhs_idx, -rhs_idx),
                    OpBinary::Iff => {
                        let ltr = -self.bdd_make_and(lhs_idx, -rhs_idx);
                        let rtl = -self.bdd_make_and(-lhs_idx, rhs_idx);
                        self.bdd_make_and(ltr, rtl)
                    }
                }
            }

            Formula::Quantifier { .. } => todo!(),
        }
    }

    pub fn representation_string(&self, head: BDDIndex) {
        let mut stack: Vec<BDDIndex> = Vec::default();
        let mut next = Some(head);

        let mut end = false;

        let spacing = 2;
        let mut indent = 0;
        let mut depth = 0;

        while let Some(idx) = next {
            match idx {
                1 => {
                    println!(" ⊤");
                    next = stack.pop();

                    depth -= 1;
                    end = true;
                }

                -1 => {
                    for _ in 0..(indent - depth - 1) {
                        print!("    ");
                    }
                    match depth {
                        0 => {}
                        1 => print!("  │ "),
                        _ => {
                            for _ in 0..depth {
                                print!("  │ ")
                            }
                        }
                    }

                    println!("  └  ⊥");
                    next = stack.pop();

                    depth -= 1;
                    indent -= 1;

                    end = true;
                }

                _ => {
                    let node = self
                        .by_index
                        .get(&idx.abs())
                        .unwrap_or_else(|| panic!("!{idx}"));
                    if end {
                        for _ in 1..indent {
                            print!("    ");
                        }
                        print!("  └ ");
                    }
                    match idx.is_positive() {
                        true => print!("+ {} ", node.prop),
                        false => print!("- {} ", node.prop),
                    }

                    depth += 1;
                    indent += 1;
                    stack.push(node.fb);
                    next = Some(node.tb);
                    end = false;
                }
            }
        }
    }
}

impl PropFormula {
    pub fn bdd(self) -> (BDDIndex, BDDGraph) {
        let mut graph = BDDGraph::default();
        let head = graph.bdd_make(&self);
        (head, graph)
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::parse_propositional_formula;

    #[test]
    fn basic() {
        let expr = parse_propositional_formula("p & q & r & s");
        let (head, graph) = expr.bdd();

        graph.representation_string(head);
        println!("{head}");
        for (idx, node) in graph.by_index {
            println!("{idx} : {node}");
        }

        print!("\n\n");

        let expr = parse_propositional_formula("-p & -q");
        let (head, graph) = expr.bdd();

        graph.representation_string(head);
        println!();
        println!("{head}");
        for (idx, node) in graph.by_index {
            println!("{idx} : {node}");
        }

        print!("\n\n");

        let expr = parse_propositional_formula("p | q | r");
        let (head, graph) = expr.bdd();

        graph.representation_string(head);
        println!("{head}");
        for (idx, node) in graph.by_index {
            println!("{idx} : {node}");
        }

        print!("\n\n");

        let expr = parse_propositional_formula("p <=> (q => ~r)");
        let (head, graph) = expr.bdd();

        graph.representation_string(head);
        println!("{head}");
        for (idx, node) in graph.by_index {
            println!("{idx} : {node}");
        }

        print!("\n\n");
    }
}
