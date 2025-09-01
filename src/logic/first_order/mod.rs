use std::{collections::HashSet, iter::Peekable};

use crate::logic::parse::Token;

pub type TermId = String;

#[derive(Clone, Debug)]
pub enum Term {
    /// A constant, or function with no arguments
    Cst { id: TermId },
    /// A variable
    Var { id: TermId },
    /// A function, with at least one argument
    Fun { id: TermId, args: Vec<Term> },
}

impl Term {
    pub fn variable(id: &str) -> Self {
        Term::Var { id: id.to_owned() }
    }

    pub fn constant(id: &str) -> Self {
        Term::Cst { id: id.to_owned() }
    }

    pub fn function(id: &str, args: &[Term]) -> Self {
        Term::Fun {
            id: id.to_owned(),
            args: args.to_vec(),
        }
    }

    pub fn unary(op: &str, term: Term) -> Self {
        Term::function(op, &[term])
    }

    pub fn binary(op: &str, lhs: Term, rhs: Term) -> Self {
        Term::function(op, &[lhs, rhs])
    }

    pub fn is_const_id(id: &str) -> bool {
        match id {
            "nil" => true,
            _ => id.chars().all(|c| c.is_numeric()),
        }
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Cst { id } => write!(f, "{id}"),
            Term::Var { id } => write!(f, "{id}"),
            Term::Fun { id, args } => match args.as_slice() {
                [] => write!(f, "{}", id),
                [first, remaining @ ..] => {
                    let mut arg_string = format!("{first}");
                    for arg in remaining {
                        arg_string.push_str(&format!(", {arg}"));
                    }

                    write!(f, "{}({arg_string})", id)
                }
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct Relation {
    id: TermId,
    terms: Vec<Term>,
}

impl Relation {
    pub fn predicate(id: &str) -> Self {
        Self {
            id: id.to_owned(),
            terms: Vec::default(),
        }
    }

    pub fn n_ary(id: &str, terms: &[Term]) -> Self {
        Self {
            id: id.to_owned(),
            terms: terms.to_vec(),
        }
    }
}

impl std::fmt::Display for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.terms.as_slice() {
            [] => write!(f, "{}", self.id),
            [first, remaining @ ..] => {
                let mut term_string = format!("{first}");
                for term in remaining {
                    term_string.push_str(&format!(", {term}"));
                }

                write!(f, "({}, {term_string})", self.id)
            }
        }
    }
}

fn try_parse_term<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &HashSet<TermId>,
) -> Option<Term> {
    let id = match tokens.peek() {
        Some(Token::Identifier(id)) => id.to_owned(),
        _ => return None,
    };
    tokens.next();

    let mut args: Vec<Term> = Vec::default();

    match tokens.next_if(|t| matches!(t, Token::ParenL(_))) {
        Some(Token::ParenL(l)) => {
            loop {
                match tokens.peek() {
                    Some(Token::ParenR(r)) if &l == r => {
                        tokens.next();
                        break;
                    }

                    Some(Token::ParenR(_)) => panic!("Mismatched parentheses"),

                    Some(Token::Comma) => {
                        tokens.next();
                    }

                    _ => match try_parse_term(tokens, variable_ids) {
                        Some(term) => args.push(term),
                        None => panic!("Hm"),
                    },
                }
            }
            match args.is_empty() {
                true => Some(Term::Cst { id }),
                false => Some(Term::Fun { id, args }),
            }
        }

        _ => {
            if variable_ids.contains(&id) || !Term::is_const_id(&id) {
                Some(Term::Var { id })
            } else {
                Some(Term::Cst { id })
            }
        }
    }
}

fn try_parse_relation<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &HashSet<TermId>,
) -> Option<Relation> {
    let id = match tokens.peek() {
        Some(Token::Identifier(id)) => id.to_owned(),
        _ => return None,
    };
    tokens.next();

    let mut terms: Vec<Term> = Vec::default();

    if let Some(Token::ParenL(l)) = tokens.next_if(|t| matches!(t, Token::ParenL(_))) {
        loop {
            match tokens.peek() {
                Some(Token::ParenR(r)) if &l == r => {
                    break;
                }

                Some(Token::ParenR(_)) => panic!("Mismatched parentheses"),

                Some(Token::Comma) => {
                    tokens.next();
                }

                _ => match try_parse_term(tokens, variable_ids) {
                    Some(term) => terms.push(term),
                    None => break,
                },
            }
        }
    }

    Some(Relation { id, terms })
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::{
        first_order::{try_parse_relation, try_parse_term, Term, TermId},
        parse::lex,
    };

    #[test]
    fn debug_term() {
        let variable_ids: HashSet<TermId> = HashSet::default();

        let _term = Term::unary(
            "sqrt",
            Term::binary(
                "-",
                Term::constant("1"),
                Term::unary(
                    "cos",
                    Term::binary(
                        "pow",
                        Term::binary("+", Term::variable("x"), Term::variable("y")),
                        Term::constant("2"),
                    ),
                ),
            ),
        );

        // println!("{term}");

        let str = "a";
        let mut tokens = lex(str).into_iter().peekable();
        let tmp = try_parse_term(&mut tokens, &variable_ids).unwrap();
        println!("{tmp}");

        let str = "f(a,g(b,h(c)))";
        let mut tokens = lex(str).into_iter().peekable();
        let tmp = try_parse_term(&mut tokens, &variable_ids).unwrap();
        println!("{tmp}");
    }

    #[test]
    fn debug_relation() {
        // let x_plus_y = Term::binary("+", Term::variable("x"), Term::variable("y"));
        // let z = Term::variable("z");
        // let r = Relation::n_ary("<", &[x_plus_y, z]);

        let variable_ids: HashSet<TermId> = HashSet::default();

        let str = "R()";
        let mut tokens = lex(str).into_iter().peekable();
        let tmp = try_parse_relation(&mut tokens, &variable_ids).unwrap();
        println!("{tmp}");

        let str = "EQ(add(a,b), times(minus(x),y))";
        let mut tokens = lex(str).into_iter().peekable();
        let tmp = try_parse_relation(&mut tokens, &variable_ids).unwrap();
        println!("{tmp}");
    }
}
