use std::{collections::HashSet, iter::Peekable};

use crate::logic::{
    first_order::{Relation, Term, TermId},
    parse::Token,
};

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

    Some(Relation::from(id, terms))
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::logic::{
        first_order::{Term, TermId, },
        parse::{first_order::{try_parse_relation, try_parse_term}, lex},
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
