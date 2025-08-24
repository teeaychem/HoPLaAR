use std::iter::Peekable;

use crate::arithmetic::Expr;

#[derive(Clone, Copy, Debug)]
enum OpBinary {
    Add,
    Mul,
}

#[derive(Clone, Debug)]
enum Token {
    BraceL,
    BraceR,
    BracketL,
    BracketR,
    ParenL,
    ParenR,
    Number(i64),
    OpBinary(OpBinary),
    Var(String),
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
            '+' => tokens.push(Token::OpBinary(OpBinary::Add)),
            '*' => tokens.push(Token::OpBinary(OpBinary::Mul)),

            digit if digit.is_ascii_digit() => {
                let mut digits = String::from(digit);
                while let Some(char) = chars.peek() {
                    match char.is_ascii_digit() {
                        true => {
                            digits.push(*char);
                            chars.next();
                        }
                        false => break,
                    }
                }
                let number = digits.parse().unwrap();
                tokens.push(Token::Number(number))
            }

            char if char.is_ascii_alphanumeric() => {
                let mut id = String::from(char);
                while let Some(char) = chars.peek() {
                    match char {
                        a if a.is_alphanumeric() => {
                            id.push(*char);
                            chars.next();
                        }
                        '_' | '\'' | '’' => {
                            id.push(*char);
                            chars.next();
                        }
                        _ => break,
                    }
                }
                tokens.push(Token::Var(id))
            }

            whitespace if whitespace.is_whitespace() => {}
            _ => {
                todo!()
            }
        }
    }

    tokens
}

fn parse_expression<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> Expr {
    let expression = parse_product(tokens);

    match tokens.peek() {
        Some(Token::OpBinary(OpBinary::Add)) => {
            tokens.next();
            let rhs = parse_expression(tokens);

            Expr::Add {
                lhs: Box::new(expression),
                rhs: Box::new(rhs),
            }
        }

        _ => expression,
    }
}

fn parse_product<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> Expr {
    let expression = parse_atom(tokens);

    match tokens.peek() {
        Some(Token::OpBinary(OpBinary::Mul)) => {
            tokens.next();
            let rhs = parse_product(tokens);

            Expr::Mul {
                lhs: Box::new(expression),
                rhs: Box::new(rhs),
            }
        }

        _ => expression,
    }
}

fn parse_atom<I: Iterator<Item = Token>>(tokens: &mut Peekable<I>) -> Expr {
    match tokens.next() {
        Some(Token::ParenL) => {
            let expression = parse_expression(tokens);
            match tokens.next() {
                Some(Token::ParenR) => expression,
                _ => panic!("Expected closing parenethsis"),
            }
        }

        Some(Token::BracketL) => {
            let expression = parse_expression(tokens);
            match tokens.next() {
                Some(Token::BracketR) => expression,
                _ => panic!("Expected closing parenethsis"),
            }
        }

        Some(Token::BraceL) => {
            let expression = parse_expression(tokens);
            match tokens.next() {
                Some(Token::BraceR) => expression,
                _ => panic!("Expected closing parenethsis"),
            }
        }

        Some(Token::Number(val)) => Expr::Const { val },

        Some(Token::Var(id)) => Expr::Var { id },

        None => panic!("Expected an expression at end of input"),

        Some(unexpected) => panic!("Unexpected token: {unexpected:?}"),
    }
}

#[cfg(test)]
mod tests {
    use crate::arithmetic::parsing::{lex, parse_expression};

    #[test]
    fn debug() {
        let string = "2*((var_1 + x’) + 11)";
        let lex = lex(string);
        println!("{:?}", lex);

        let mut tokens = lex.into_iter().peekable();

        println!("{}", parse_expression(&mut tokens));
    }
}
