mod propositional;
pub use propositional::parse_propositional;

mod first_order;
#[allow(unused_imports)]
pub use first_order::{parse_first_order, try_parse_relation, try_parse_term};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Paren {
    Blinky,
    Pinky,
    Inky,
    Clyde,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Quantifier {
    ForAll,
    Exists,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Token {
    ParenL(Paren),
    ParenR(Paren),
    Identifier(String),
    True,
    False,
    Not,
    And,
    Or,
    Imp,
    Iff,
    Comma,
    Stop,
    Quantifier(Quantifier),
    Whitespace,
}

type TokenVec = Vec<Token>;

static DELIMITING_CHARS: &str = "(){}[],.~&|\\/=<";

fn string_to_token(string: &str) -> Token {
    match string {
        "true" => Token::True,
        "false" => Token::False,
        "not" => Token::Not,
        "and" => Token::And,
        "or" => Token::Or,
        "implies" => Token::Imp,
        "iff" => Token::Iff,
        "forall" => Token::Quantifier(Quantifier::ForAll),
        "exists" => Token::Quantifier(Quantifier::Exists),
        _ => {
            let mut chars = string.chars();

            let head_ok = chars.next().is_some_and(|head| head.is_alphanumeric());
            let body_ok = chars.all(|char| char.is_alphanumeric() || "_\'`".contains(char));
            if head_ok && body_ok {
                Token::Identifier(string.to_owned())
            } else {
                panic!("Failed identifier: '{string}'")
            }
        }
    }
}

fn lex(expr: &str) -> TokenVec {
    let mut tokens = Vec::default();

    let mut chars = expr.chars().peekable();

    while let Some(char) = chars.next() {
        let token = match char {
            whitespace if whitespace.is_whitespace() => {
                while chars.peek().is_some_and(|c| c.is_whitespace()) {
                    chars.next();
                }

                Token::Whitespace
            }

            '(' => Token::ParenL(Paren::Blinky),
            ')' => Token::ParenR(Paren::Blinky),

            '[' => Token::ParenL(Paren::Pinky),
            ']' => Token::ParenR(Paren::Pinky),

            '{' => Token::ParenL(Paren::Inky),
            '}' => Token::ParenR(Paren::Inky),

            '~' => Token::Not,

            '|' => Token::Or,
            '&' => Token::And,

            '/' => match chars.next() {
                Some('\\') => Token::And,
                _ => panic!(),
            },

            '\\' => match chars.next() {
                Some('/') => Token::Or,
                _ => panic!(),
            },

            '=' => match chars.next() {
                Some('>') => Token::Imp,
                Some('=') => match chars.peek() {
                    Some('>') => {
                        chars.next();
                        Token::Imp
                    }
                    _ => Token::Iff,
                },
                _ => panic!(),
            },

            '<' => match chars.next() {
                Some('=') => match chars.next() {
                    Some('>') => Token::Iff,
                    _ => panic!(),
                },
                _ => panic!(),
            },

            ',' => Token::Comma,
            '.' => Token::Stop,

            char => {
                let mut string = String::from(char);
                while let Some(char) = chars.peek() {
                    match char {
                        whitespace if whitespace.is_whitespace() => break,
                        delimiting if DELIMITING_CHARS.contains(*delimiting) => break,
                        c => {
                            string.push(*c);
                            chars.next();
                        }
                    }
                }
                string_to_token(&string)
            }
        };

        match token {
            Token::Whitespace => {}
            _ => tokens.push(token),
        }
    }

    tokens
}
