use std::{collections::HashSet, iter::Peekable};

use crate::logic::{
    first_order::{FirstOrderFormula, Relation, Term, TermId},
    parse::{Token, lex},
};

pub fn parse_first_order(str: &str) -> FirstOrderFormula {
    let mut tokens = lex(str).into_iter().peekable();
    let mut variable_ids = HashSet::default();
    parse_iff(&mut tokens, &mut variable_ids)
}

fn parse_iff<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    let expression = parse_imp(tokens, variable_ids);

    match tokens.peek() {
        Some(Token::Iff) => {
            tokens.next();
            let rhs = parse_iff(tokens, variable_ids);
            FirstOrderFormula::Iff(expression, rhs)
        }

        _ => expression,
    }
}

fn parse_imp<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    let expression = parse_or(tokens, variable_ids);

    match tokens.peek() {
        Some(Token::Imp) => {
            tokens.next();
            let rhs = parse_imp(tokens, variable_ids);
            FirstOrderFormula::Imp(expression, rhs)
        }

        _ => expression,
    }
}

fn parse_or<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    let expression = parse_and(tokens, variable_ids);

    match tokens.peek() {
        Some(Token::Or) => {
            tokens.next();
            let rhs = parse_or(tokens, variable_ids);
            FirstOrderFormula::Or(expression, rhs)
        }

        _ => expression,
    }
}

fn parse_and<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    let expression = parse_base(tokens, variable_ids);

    match tokens.peek() {
        Some(Token::And) => {
            tokens.next();
            let rhs = parse_and(tokens, variable_ids);
            FirstOrderFormula::And(expression, rhs)
        }

        _ => expression,
    }
}

fn parse_base<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    // In order to avoid consuming the identifier of a relation, advance to the next token only if the token is not an identifier.
    tokens.next_if(|t| !matches!(t, Token::Identifier(_)));
    match tokens.peek() {
        Some(Token::ParenL(l)) => {
            let paren_kind = *l; // As peek borrows, create a clone of `l` to release the borrow and permit mutation of the token vec
            let expression = parse_iff(tokens, variable_ids);
            match tokens.next() {
                Some(Token::ParenR(r)) if paren_kind == r => {}
                _ => panic!("Expected closing parenethsis"),
            }

            expression
        }

        Some(Token::True) => FirstOrderFormula::True,

        Some(Token::False) => FirstOrderFormula::False,

        Some(Token::Not) => {
            let expr = parse_base(tokens, variable_ids);
            FirstOrderFormula::Not(expr)
        }

        Some(Token::Identifier(_)) => match try_parse_relation(tokens, variable_ids) {
            Some(relation) => FirstOrderFormula::Atom(relation),
            None => panic!(),
        },

        None => panic!("Expected an expression at end of input"),

        Some(unexpected) => panic!("Unexpected token: {unexpected:?}"),
    }
}

fn try_parse_relation<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
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

fn try_parse_term<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
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

#[cfg(test)]
mod tests {

    use crate::logic::{first_order::Term, parse::first_order::parse_first_order};

    #[test]
    fn debug() {
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

        let expr = "a & f(f(x))";
        let tmp = parse_first_order(expr);
        println!("{tmp}");

        // let expr = "f(a,g(b,h(c)))";
        // let tmp = parse_first_order(expr);
        // println!("{tmp}");

        // let x_plus_y = Term::binary("+", Term::variable("x"), Term::variable("y"));
        // let z = Term::variable("z");
        // let r = Relation::n_ary("<", &[x_plus_y, z]);

        let expr = "R()";
        let tmp = parse_first_order(expr);
        println!("{tmp}");

        let expr = "EQ(add(a,b), times(minus(x),y))";
        let tmp = parse_first_order(expr);
        println!("{tmp}");
    }
}
