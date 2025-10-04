use std::{collections::HashSet, iter::Peekable};

use crate::logic::{
    first_order::{FirstOrderFormula, Relation, Term, TermId},
    parse::{Quantifier, Token, lex},
};

impl From<&str> for FirstOrderFormula {
    fn from(value: &str) -> Self {
        let mut tokens = lex(value).into_iter().peekable();
        let mut variable_ids = HashSet::default();
        parse_iff(&mut tokens, &mut variable_ids)
    }
}

fn parse_iff<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    let expr = parse_imp(tokens, variable_ids);

    match tokens.peek() {
        Some(Token::Iff) => {
            tokens.next();
            let rhs = parse_iff(tokens, variable_ids);
            FirstOrderFormula::Iff(expr, rhs)
        }

        _ => expr,
    }
}

fn parse_imp<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    let expr = parse_or(tokens, variable_ids);

    match tokens.peek() {
        Some(Token::Imp) => {
            tokens.next();
            let rhs = parse_imp(tokens, variable_ids);
            FirstOrderFormula::Imp(expr, rhs)
        }

        _ => expr,
    }
}

fn parse_or<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    let expr = parse_and(tokens, variable_ids);

    match tokens.peek() {
        Some(Token::Or) => {
            tokens.next();
            let rhs = parse_or(tokens, variable_ids);
            FirstOrderFormula::Or(expr, rhs)
        }

        _ => expr,
    }
}

fn parse_and<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    let expr = parse_base(tokens, variable_ids);

    match tokens.peek() {
        Some(Token::And) => {
            tokens.next();
            let rhs = parse_and(tokens, variable_ids);
            FirstOrderFormula::And(expr, rhs)
        }

        _ => expr,
    }
}

fn parse_base<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    variable_ids: &mut HashSet<TermId>,
) -> FirstOrderFormula {
    // In order to avoid consuming the identifier of a relation, advance to the next token only if the token is not an identifier.

    match tokens.peek() {
        Some(Token::ParenL(l)) => {
            let paren_kind = *l; // As peek borrows, create a clone of `l` to release the borrow and permit mutation of the token vec
            tokens.next();
            let expr = parse_iff(tokens, variable_ids);
            match tokens.next() {
                Some(Token::ParenR(r)) if paren_kind == r => {}
                _ => panic!("Expected closing parenethsis"),
            }

            expr
        }

        Some(Token::True) => {
            tokens.next();
            FirstOrderFormula::True
        }

        Some(Token::False) => {
            tokens.next();
            FirstOrderFormula::False
        }

        Some(Token::Not) => {
            tokens.next();
            let expr = parse_base(tokens, variable_ids);
            FirstOrderFormula::Not(expr)
        }

        Some(Token::Identifier(_)) => match try_parse_relation_local(tokens, variable_ids) {
            Some(relation) => FirstOrderFormula::Atom(relation),
            None => panic!(),
        },

        Some(Token::Quantifier(q)) => {
            let q = *q;

            tokens.next();
            let var = match try_parse_term_local(tokens, variable_ids) {
                Some(Term::V(var)) => var,
                Some(Term::F(fun)) => {
                    panic!("Unable to use function / constant '{fun}' as a variable")
                }

                None => panic!("Expected a variable to follow {q:?}"),
            };

            tokens.next_if(|t| matches!(t, Token::Stop));

            variable_ids.insert(var.id.to_owned());
            let expr = parse_base(tokens, variable_ids);
            variable_ids.remove(&var.id);

            match q {
                Quantifier::ForAll => FirstOrderFormula::ForAll(var, expr),
                Quantifier::Exists => FirstOrderFormula::Exists(var, expr),
            }
        }

        None => panic!("Expected an expression at end of input"),

        Some(unexpected) => panic!("Unexpected token: {unexpected:?}"),
    }
}

pub fn try_parse_relation_local<I: Iterator<Item = Token>>(
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
                    tokens.next();
                    break;
                }

                Some(Token::ParenR(_)) => panic!("Mismatched parentheses"),

                Some(Token::Comma) => {
                    tokens.next();
                }

                _ => match try_parse_term_local(tokens, variable_ids) {
                    Some(term) => terms.push(term),
                    None => break,
                },
            }
        }
    }

    Some(Relation::from(id, terms))
}

impl TryFrom<&str> for Relation {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut tokens = lex(value).into_iter().peekable();
        let mut variable_ids = HashSet::default();
        match try_parse_relation_local(&mut tokens, &mut variable_ids) {
            Some(relation) => Ok(relation),
            None => Err(()),
        }
    }
}

fn is_const_id(id: &TermId) -> bool {
    match id.as_str() {
        "nil" => true,
        _ => id.chars().all(|c| c.is_numeric()),
    }
}

fn try_parse_term_local<I: Iterator<Item = Token>>(
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

                    _ => match try_parse_term_local(tokens, variable_ids) {
                        Some(term) => args.push(term),
                        None => panic!("Hm"),
                    },
                }
            }
            match args.is_empty() {
                true => Some(Term::Cst(&id)),
                false => Some(Term::Fun(&id, &args)),
            }
        }

        _ => {
            if variable_ids.contains(&id) || !is_const_id(&id) {
                Some(Term::Var(&id))
            } else {
                Some(Term::Cst(&id))
            }
        }
    }
}

impl TryFrom<&str> for Term {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut tokens = lex(value).into_iter().peekable();
        let mut variable_ids = HashSet::default();
        match try_parse_term_local(&mut tokens, &mut variable_ids) {
            Some(term) => Ok(term),
            None => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::first_order::{FirstOrderFormula, library::pelletier};

    #[test]
    fn debug() {
        let f = FirstOrderFormula::from(pelletier::P22);
        println!("{f}");
        let r = f.tableaux(None);
        println!("{r:?}");

        // let _term = Term::unary(
        //     "sqrt",
        //     Term::binary(
        //         "-",
        //         Term::Cst("1"),
        //         Term::unary(
        //             "cos",
        //             Term::binary(
        //                 "pow",
        //                 Term::binary("+", Term::Var("x"), Term::Var("y")),
        //                 Term::Cst("2"),
        //             ),
        //         ),
        //     ),
        // );

        // // println!("{term}");

        // let expr = "a & f(f(x))";
        // let tmp = parse_first_order(expr);
        // println!("{tmp}");

        // // let expr = "f(a,g(b,h(c)))";
        // // let tmp = parse_first_order(expr);
        // // println!("{tmp}");

        // // let x_plus_y = Term::binary("+", Term::variable("x"), Term::variable("y"));
        // // let z = Term::variable("z");
        // // let r = Relation::n_ary("<", &[x_plus_y, z]);

        // let expr = "EQ(add(a,b), times(minus(x),y))";
        // let tmp = parse_first_order(expr);
        // println!("{tmp}");

        // let expr = "~forall x P(f(1)) | exists 1 ~P(1) & R(0,1)";
        // let _ = parse_first_order(expr);
        // dbg!(&tmp);
        // println!("{tmp}");
    }
}
