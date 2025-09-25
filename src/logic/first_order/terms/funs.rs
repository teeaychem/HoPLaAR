//! Functions (and constants)
//!
//! <div class="warning">
//! Equality of [Fun]'s is derived from equality of id, variant, and arity.
//! Equality of `Fun`'s is *not* determined by argument instances.
//!
//! For example, *f(x)* == *f(y)*.
//!</div>
//!
//! As a consequence of the hashing of [Fun]'s is (also) independent of argument instances.

use crate::logic::first_order::{Term, TermId, terms::Variant};

#[derive(Clone, Debug)]
pub struct Fun {
    pub id: TermId,
    pub variant: Variant,
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
        use std::cmp::Ordering;

        match self.arity().cmp(&other.arity()) {
            Ordering::Equal => {}
            ord => return ord,
        }

        match self.id.cmp(&other.id) {
            Ordering::Equal => {}
            ord => return ord,
        }

        match self.variant.cmp(&other.variant) {
            Ordering::Equal => {}
            ord => return ord,
        }

        Ordering::Equal
    }
}

impl PartialEq for Fun {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.variant == other.variant && self.arity() == other.arity()
    }
}

impl Eq for Fun {}

impl From<&Fun> for Term {
    fn from(value: &Fun) -> Self {
        Term::F(value.clone())
    }
}

impl From<Fun> for Term {
    fn from(value: Fun) -> Self {
        Term::F(value)
    }
}

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

impl Fun {
    pub fn fmt_ansi(&self, f: &mut std::fmt::Formatter<'_>, ansi: bool) -> std::fmt::Result {
        use std::fmt::Write;
        let mut s = String::default();

        if ansi {
            match self.arity() {
                0 => write!(f, "\x1B[1m")?,
                _ => write!(f, "\x1B[3m")?,
            }
        }

        write!(f, "{}", self.id)?;

        if 0 < self.variant {
            write!(s, "_{}", self.variant)?
        }

        if ansi {
            write!(f, "\x1B[0m")?;
        }

        match ansi {
            true => match self.args.as_slice() {
                [] => {}
                [first, remaining @ ..] => {
                    write!(f, "(")?;
                    first.fmt_ansi(f, ansi)?;
                    for arg in remaining {
                        write!(f, ", ")?;
                        arg.fmt_ansi(f, ansi)?;
                    }
                    write!(f, ")")?
                }
            },

            false => {
                write!(f, "(")?;
                match self.args.as_slice() {
                    [] => {}
                    [first, remaining @ ..] => {
                        first.fmt_ansi(f, ansi)?;
                        for arg in remaining {
                            write!(f, ", ")?;
                            arg.fmt_ansi(f, ansi)?;
                        }
                    }
                }
                write!(f, ")")?
            }
        }

        Ok(())
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
