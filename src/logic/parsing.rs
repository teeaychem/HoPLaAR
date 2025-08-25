use std::iter::Peekable;

use crate::logic::{Prop, PropFormula};

#[derive(Clone, Debug)]
enum Token {
    BraceL,
    BraceR,
    BracketL,
    BracketR,
    ParenL,
    ParenR,
    Var(String),
    True,
    False,
    Not,
    And,
    Or,
    Imp,
    Iff,
}

type TokenVec = Vec<Token>;

fn lex(expr: &str) -> Vec<Token> {
    let mut tokens = Vec::default();

    let mut chars = expr.chars().peekable();

    while let Some(char) = chars.next() {
        match char {
            '(' => tokens.push(Token::ParenL),
            ')' => tokens.push(Token::ParenR),
            '[' => tokens.push(Token::BracketL),
            ']' => tokens.push(Token::BracketR),
            '{' => tokens.push(Token::BraceL),
            '}' => tokens.push(Token::BraceR),

            '1' | 'T' => tokens.push(Token::True),
            '0' | 'F' => tokens.push(Token::False),

            '-' | '~' => tokens.push(Token::Not),

            '+' | '|' => tokens.push(Token::Or),
            '*' | '&' => tokens.push(Token::And),

            '/' => match chars.next() {
                Some('\\') => tokens.push(Token::And),
                _ => todo!(),
            },

            '\\' => match chars.next() {
                Some('/') => tokens.push(Token::Or),
                _ => todo!(),
            },

            '=' => match chars.next() {
                Some('>') => tokens.push(Token::Imp),
                Some('=') => match chars.peek() {
                    Some('>') => {
                        chars.next();
                        tokens.push(Token::Imp);
                    }

                    _ => tokens.push(Token::Iff),
                },
                _ => todo!(),
            },

            '<' => match chars.next() {
                Some('=') => match chars.next() {
                    Some('>') => tokens.push(Token::Iff),
                    _ => todo!(),
                },
                _ => todo!(),
            },

            char if char.is_ascii_alphanumeric() => {
                let mut string = String::from(char);
                while let Some(char) = chars.peek() {
                    match char {
                        a if a.is_alphanumeric() => {
                            string.push(*char);
                            chars.next();
                        }
                        '_' | '\'' | '’' => {
                            string.push(*char);
                            chars.next();
                        }
                        _ => break,
                    }
                }

                match string.as_str() {
                    "true" => tokens.push(Token::True),
                    "false" => tokens.push(Token::False),
                    "not" => tokens.push(Token::Not),
                    "and" => tokens.push(Token::And),
                    "or" => tokens.push(Token::Or),
                    "implies" => tokens.push(Token::Imp),
                    "iff" => tokens.push(Token::Iff),
                    _ => tokens.push(Token::Var(string)),
                }
            }

            whitespace if whitespace.is_whitespace() => {}
            _ => {
                todo!()
            }
        }
    }

    tokens
}

pub fn parse_propositional_formula(str: &str) -> PropFormula {
    let mut tokens = lex(str).into_iter().peekable();
    parse_formula(&mut tokens)
}

fn parse_formula<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> PropFormula {
    parse_iff(tokens)
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
        Some(Token::ParenL) => {
            let expression = parse_formula(tokens);
            match tokens.next() {
                Some(Token::ParenR) => expression,
                _ => panic!("Expected closing parenethsis"),
            }
        }

        Some(Token::BracketL) => {
            let expression = parse_formula(tokens);
            match tokens.next() {
                Some(Token::BracketR) => expression,
                _ => panic!("Expected closing parenethsis"),
            }
        }

        Some(Token::BraceL) => {
            let expression = parse_formula(tokens);
            match tokens.next() {
                Some(Token::BraceR) => expression,
                _ => panic!("Expected closing parenethsis"),
            }
        }

        Some(Token::True) => PropFormula::True,

        Some(Token::False) => PropFormula::False,

        Some(Token::Not) => {
            let expr = parse_formula(tokens);
            PropFormula::Not(expr)
        }

        Some(Token::Var(name)) => PropFormula::Atom(Prop { name }),

        None => panic!("Expected an expression at end of input"),

        Some(unexpected) => panic!("Unexpected token: {unexpected:?}"),
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{Prop, PropFormula, parsing::parse_propositional_formula};

    #[test]
    fn simple() {
        let name = "a";
        let formula = parse_propositional_formula(name);
        assert_eq!(formula, PropFormula::Atom(Prop::new(name)));

        let expr = "true & false";
        let parse = parse_propositional_formula(expr);
        assert_eq!(
            parse,
            PropFormula::And(PropFormula::True, PropFormula::False)
        );
    }
}
