use std::iter::Peekable;

use crate::logic::{
    parse::{Token, lex},
    propositional::{Prop, PropFormula},
};

pub fn parse_propositional(str: &str) -> PropFormula {
    let mut tokens = lex(str).into_iter().peekable();
    parse_iff(&mut tokens)
}

fn parse_iff<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> PropFormula {
    let expression = parse_imp(tokens);

    match tokens.peek() {
        Some(Token::Iff) => {
            tokens.next();
            let rhs = parse_iff(tokens);
            PropFormula::Iff(expression, rhs)
        }

        _ => expression,
    }
}

fn parse_imp<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> PropFormula {
    let expression = parse_or(tokens);

    match tokens.peek() {
        Some(Token::Imp) => {
            tokens.next();
            let rhs = parse_imp(tokens);
            PropFormula::Imp(expression, rhs)
        }

        _ => expression,
    }
}

fn parse_or<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> PropFormula {
    let expression = parse_and(tokens);

    match tokens.peek() {
        Some(Token::Or) => {
            tokens.next();
            let rhs = parse_or(tokens);
            PropFormula::Or(expression, rhs)
        }

        _ => expression,
    }
}

fn parse_and<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> PropFormula {
    let expression = parse_atom(tokens);

    match tokens.peek() {
        Some(Token::And) => {
            tokens.next();
            let rhs = parse_and(tokens);
            PropFormula::And(expression, rhs)
        }

        _ => expression,
    }
}

fn parse_atom<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> PropFormula {
    match tokens.next() {
        Some(Token::ParenL(l_kind)) => {
            let expression = parse_iff(tokens);
            match tokens.next() {
                Some(Token::ParenR(r_kind)) if l_kind == r_kind => {}
                _ => panic!("Expected closing parenethsis"),
            }

            expression
        }

        Some(Token::True) => PropFormula::True,

        Some(Token::False) => PropFormula::False,

        Some(Token::Not) => {
            let expr = parse_atom(tokens);
            PropFormula::Not(expr)
        }

        Some(Token::Identifier(id)) => PropFormula::Atom(Prop::from(&id)),

        None => panic!("Expected an expression at end of input"),

        Some(unexpected) => panic!("Unexpected token: {unexpected:?}"),
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{
        parse_propositional,
        propositional::{Prop, PropFormula},
    };

    #[test]
    fn simple() {
        let id = "a";
        let formula = parse_propositional(id);
        assert_eq!(formula, PropFormula::Atom(Prop::from(id)));

        let expr = "true & false";
        let parse = parse_propositional(expr);
        assert_eq!(
            parse,
            PropFormula::And(PropFormula::True, PropFormula::False)
        );
    }

    #[test]
    fn less_simple() {
        let expr = "p & q | r";
        let expected = "(p ∧ q) ∨ r";
        assert_eq!(format!("{}", parse_propositional(expr)), expected);

        let expr = "p & q <=> r";
        let expected = "(p ∧ q) ↔ r";
        assert_eq!(format!("{}", parse_propositional(expr)), expected);

        let expr = "~p & q <=> r";
        let expected = "(¬p ∧ q) ↔ r";
        assert_eq!(format!("{}", parse_propositional(expr)), expected);

        let expr = "~(p & q) <=> r";
        let expected = "¬(p ∧ q) ↔ r";
        assert_eq!(format!("{}", parse_propositional(expr)), expected);
    }
}
