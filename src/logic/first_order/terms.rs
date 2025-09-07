use std::collections::HashSet;

use crate::logic::{
    Variable,
    first_order::{Element, Model, Valuation, eval_term},
};

pub type TermId = String;

/// Functions (and constants)
///
/// <div class="warning">
/// Equality of [Fun]'s is derived from equality of id, variant, and arity.
/// Equality of `Fun`'s is *not* determined by argument instances.
///
/// For example, *f(x)* == *f(y)*.
///</div>
///
/// As a consequence of the hashing of [Fun]'s is (also) independent of argument instances.
#[derive(Clone, Debug)]
pub struct Fun {
    pub id: TermId,
    pub variant: usize,
    pub args: Vec<Term>,
}

impl std::hash::Hash for Fun {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.variant.hash(state);
        self.arity().hash(state);
    }
}

impl PartialOrd for Fun {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Fun {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.arity().cmp(&other.arity()) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }

        match self.id.cmp(&other.id) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }

        match self.variant.cmp(&other.variant) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }

        self.args.cmp(&other.args)
    }
}

impl PartialEq for Fun {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.variant == other.variant && self.arity() == other.arity()
    }
}

impl Eq for Fun {}

impl Fun {
    pub fn arity(&self) -> usize {
        self.args.len()
    }

    pub fn fresh_variant<'a, T: Iterator<Item = &'a Fun>>(&'a self, taken: T) -> Fun {
        let mut minimal_variant = None;

        for var in taken {
            if var == self {
                // Note, argument instances ignored
                if let Some(minimal) = minimal_variant {
                    minimal_variant = Some(std::cmp::max(minimal, var.variant));
                } else {
                    minimal_variant = Some(var.variant)
                }
            }
        }

        if let Some(minimal) = minimal_variant {
            minimal_variant = Some(minimal + 1);
        }

        Fun {
            id: self.id.clone(),
            variant: minimal_variant.unwrap_or_default(),
            args: self.args.clone(),
        }
    }

    pub fn string_ansi(&self) -> String {
        use std::fmt::Write;
        let mut s = String::default();

        let _ = write!(s, "\x1B[1m{}\x1B[0m", self.id);
        if 0 < self.variant {
            let _ = write!(s, "\x1B[1m_{}\x1B[0m", self.variant);
        }

        match self.args.as_slice() {
            [] => {}
            [first, remaining @ ..] => {
                let _ = write!(s, "(");
                let _ = write!(s, "{first}");
                for arg in remaining {
                    let _ = write!(s, ", {arg}");
                }
                let _ = write!(s, ")");
            }
        }

        s
    }
}

impl std::fmt::Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if 0 < self.variant {
            write!(f, "_{}", self.variant)?;
        }
        write!(f, "(")?;

        match self.args.as_slice() {
            [] => {}
            [first, remaining @ ..] => {
                write!(f, "{first}")?;
                for arg in remaining {
                    write!(f, ", {arg}")?;
                }
            }
        }

        write!(f, ")")?;
        Ok(())
    }
}

impl TryInto<Fun> for Term {
    type Error = ();

    fn try_into(self) -> Result<Fun, Self::Error> {
        match self {
            Term::F(fun) => Ok(fun),
            Term::V(_) => Err(()),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Var {
    pub id: TermId,
    pub variant: usize,
}

impl Variable for Var {
    fn fmt_ansi(&self, f: &mut std::fmt::Formatter<'_>, ansi: bool) -> std::fmt::Result {
        match ansi {
            true => {
                write!(f, "\x1B[3m{}\x1B[0m", self.id)?;
                if 0 < self.variant {
                    write!(f, "\x1B[3m_{}\x1B[0m", self.variant)?;
                }
            }

            false => {
                write!(f, "{}", self.id)?;
                if 0 < self.variant {
                    write!(f, "_{}", self.variant)?;
                }
            }
        }

        Ok(())
    }
}

impl From<&str> for Var {
    fn from(value: &str) -> Self {
        let (id, variant) = id_and_variant(value);
        Var { id, variant }
    }
}

impl Var {
    pub fn fresh_variant<'a, T: Iterator<Item = &'a Var>>(&'a self, taken: T) -> Var {
        let mut minimal_variant = None;

        for var in taken {
            if var.id == self.id {
                if let Some(minimal) = minimal_variant {
                    minimal_variant = Some(std::cmp::max(minimal, var.variant));
                } else {
                    minimal_variant = Some(var.variant)
                }
            }
        }

        let variant = match minimal_variant {
            Some(n) => n + 1,
            None => 0,
        };

        Var {
            id: self.id.clone(),
            variant,
        }
    }

    pub fn string_ansi(&self) -> String {
        use std::fmt::Write;
        let mut s = String::default();
        let _ = write!(s, "\x1B[3m{}\x1B[0m", self.id);
        if 0 < self.variant {
            let _ = write!(s, "\x1B[3m_{}\x1B[0m", self.variant);
        }

        s
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_ansi(f, false)
    }
}

impl TryInto<Var> for Term {
    type Error = ();

    fn try_into(self) -> Result<Var, Self::Error> {
        match self {
            Term::F(_) => Err(()),
            Term::V(var) => Ok(var),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum Term {
    /// A function
    F(Fun),
    /// A variable
    V(Var),
}

impl Term {
    pub fn string_ansi(&self) -> String {
        match self {
            Term::F(fun) => fun.string_ansi(),
            Term::V(var) => var.string_ansi(),
        }
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::V(var) => write!(f, "{var}"),
            Term::F(fun) => write!(f, "{fun}"),
        }
    }
}

#[allow(non_snake_case)]
impl Term {
    pub fn Cst(value: &str) -> Self {
        let (id, variant) = id_and_variant(value);
        Term::F(Fun {
            id,
            variant,
            args: Vec::default(),
        })
    }

    pub fn Var(id: &str) -> Self {
        Term::V(Var::from(id))
    }

    pub fn Fun(str_id: &str, args: &[Term]) -> Self {
        let (id, variant) = id_and_variant(str_id);
        Term::F(Fun {
            id,
            variant,
            args: args.to_vec(),
        })
    }

    pub fn Fun_unary(str_id: &str, term: Term) -> Self {
        Term::Fun(str_id, &[term])
    }

    pub fn Fun_binary(str_id: &str, lhs: Term, rhs: Term) -> Self {
        Term::Fun(str_id, &[lhs, rhs])
    }
}

impl Term {
    pub fn id(&self) -> &str {
        match self {
            Term::V(Var { id, .. }) | Term::F(Fun { id, .. }) => id,
        }
    }

    pub fn variant(&self) -> usize {
        match self {
            Term::V(Var { variant, .. }) | Term::F(Fun { variant, .. }) => *variant,
        }
    }

    #[allow(non_snake_case)]
    pub fn eval<E: Element, M: Model<E>>(&self, M: &M, v: &Valuation<E>) -> E {
        eval_term(self, M, v)
    }

    pub fn variables(&self) -> HashSet<Var> {
        let mut vars = HashSet::default();

        for term in self.terms_d() {
            match term {
                Term::F(_) => {}
                Term::V(var) => {
                    vars.insert(var.to_owned());
                }
            }
        }

        vars
    }

    pub fn functions(&self) -> HashSet<Fun> {
        let mut vars = HashSet::default();

        for term in self.terms_d() {
            match term {
                Term::F(fun) => {
                    vars.insert(fun.to_owned());
                }
                Term::V(_) => {}
            }
        }

        vars
    }
}

pub struct TermIteratorD<'a> {
    stack: Vec<&'a Term>,
    expr: Option<&'a Term>,
}

impl Term {
    // An iterator of the term and all sub-terms.
    pub fn terms_d(&'_ self) -> TermIteratorD<'_> {
        TermIteratorD {
            stack: Vec::default(),
            expr: Some(self),
        }
    }
}

impl<'a> Iterator for TermIteratorD<'a> {
    type Item = &'a Term;

    fn next(&mut self) -> Option<Self::Item> {
        match self.expr {
            Some(Term::V(_)) => {
                let var = self.expr;
                self.expr = self.stack.pop();
                var
            }

            Some(Term::F(Fun { args, .. })) => {
                let fun = self.expr;
                self.stack.extend(args.iter().rev());
                self.expr = self.stack.pop();
                fun
            }

            None => None,
        }
    }
}

/// Splits a trailing `_[\d+]` from an otherwise non-empty `value` and returns the prefix and digits as a pair.
pub fn id_and_variant(value: &str) -> (String, usize) {
    let mut parts = value.split('_').enumerate().peekable();
    let mut variant = 0;

    let mut base_id = String::default();
    while let Some((idx, part)) = parts.next() {
        if parts.peek().is_none()
            && !base_id.is_empty()
            && let Ok(id_variant) = part.parse::<usize>()
        {
            variant = id_variant;
            break;
        }

        if idx != 0 {
            base_id.push('_');
        }

        base_id.push_str(part);
    }

    (base_id, variant)
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::first_order::{
        Term,
        syntax::Substitution,
        terms::{Fun, Var},
    };

    #[test]
    fn ids_and_variants() {
        let v23 = Var::from("v_23");
        assert_eq!(v23.id, "v");
        assert_eq!(v23.variant, 23);

        let one = Term::Cst("1");
        assert_eq!(one.id(), "1");
        assert_eq!(one.variant(), 0);

        let one = Term::Cst("1_1");
        assert_eq!(one.id(), "1");
        assert_eq!(one.variant(), 1);
    }

    #[test]
    fn term_variables() {
        let terms = Term::try_from("f(a,g(b),h(f(a,h(b),c,d)))")
            .unwrap()
            .terms_d()
            .count();
        assert_eq!(terms, 11);
    }

    #[test]
    fn substitution() {
        let term = Term::try_from("f(X,g(x,Y))").unwrap();
        let upper_sub = |t: Term| -> Term {
            match t {
                Term::F(_) => t,
                Term::V(var) => {
                    let recase: String = var
                        .id
                        .chars()
                        .map(|c| {
                            if c.is_uppercase() {
                                c.to_lowercase().next().unwrap()
                            } else if c.is_lowercase() {
                                c.to_uppercase().next().unwrap()
                            } else {
                                c
                            }
                        })
                        .collect();

                    Term::Var(&recase)
                }
            }
        };

        let substitution = Substitution::from_function(Box::new(upper_sub));

        let new_term = substitution.apply(term);

        let other_term = Term::try_from("f(x,g(X,y))").unwrap();

        assert_eq!(new_term, other_term);
    }

    #[test]
    fn var_variants() {
        let var = Var::from("x");

        let taken = HashSet::from(["y", "z"].map(Var::from));
        assert_eq!(var.fresh_variant(taken.iter()), Var::from("x"));

        let taken = HashSet::from(["x", "y"].map(Var::from));
        assert_eq!(var.fresh_variant(taken.iter()), Var::from("x_1"));

        let taken = HashSet::from(["x", "x_1"].map(Var::from));
        assert_eq!(var.fresh_variant(taken.iter()), Var::from("x_2"));
    }

    #[test]
    fn fun_variants() {
        let fun_f_xy: Fun = Term::try_from("f(x,y)").unwrap().try_into().unwrap();
        let fun_f_yx: Fun = Term::try_from("f(y,x)").unwrap().try_into().unwrap();
        let fun_f_zz: Fun = Term::try_from("f(z, z)").unwrap().try_into().unwrap();

        let fun_f_one: Fun = Term::try_from("f_1(x,y)").unwrap().try_into().unwrap();

        let variant = fun_f_xy.fresh_variant([fun_f_yx, fun_f_zz].iter());

        assert_eq!(variant, fun_f_one);
    }

    #[test]
    fn fun_hashes() {
        let a: Fun = Term::try_from("f(x,y)").unwrap().try_into().unwrap();
        let b: Fun = Term::try_from("f(y,x)").unwrap().try_into().unwrap();
        let c: Fun = Term::try_from("f(z, z)").unwrap().try_into().unwrap();

        let mut set = HashSet::from([a, b, c]);

        assert_eq!(set.len(), 1);

        let d: Fun = Term::try_from("f(f(x))").unwrap().try_into().unwrap();

        set.insert(d);

        assert_eq!(set.len(), 2);
    }

    #[test]
    fn fun_order() {
        let a: Fun = Term::try_from("c()").unwrap().try_into().unwrap();
        let b: Fun = Term::try_from("f(x)").unwrap().try_into().unwrap();
        let c: Fun = Term::try_from("g(x)").unwrap().try_into().unwrap();
        let d: Fun = Term::try_from("g(x, y)").unwrap().try_into().unwrap();

        let mut v = vec![d.clone(), b.clone(), a.clone(), c.clone()];
        let sorted = vec![a, b, c, d];

        assert_ne!(v, sorted);
        v.sort();
        assert_eq!(v, sorted);
    }
}
