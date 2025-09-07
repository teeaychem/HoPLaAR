use crate::logic::{
    Variable,
    first_order::{Term, TermId, terms::id_and_variant},
};

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
