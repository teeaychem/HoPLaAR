mod propositional;
pub use propositional::parse_propositional;

mod first_order;

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
        _ => Token::Identifier(string.to_owned()),
    }
}

fn lex(expr: &str) -> TokenVec {
    let mut tokens = Vec::default();

    let mut chars = expr.chars().peekable();

    while let Some(char) = chars.next() {
        let token = match char {
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
                _ => string_to_token("/"),
            },

            '\\' => match chars.next() {
                Some('/') => Token::Or,
                _ => string_to_token("\\"),
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
                _ => string_to_token("="),
            },

            '<' => match chars.next() {
                Some('=') => match chars.next() {
                    Some('>') => Token::Iff,
                    _ => string_to_token("<="),
                },
                _ => string_to_token("<"),
            },

            ',' => Token::Comma,
            '.' => Token::Stop,

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

                string_to_token(&string)
            }

            whitespace if whitespace.is_whitespace() => Token::Whitespace,

            _ => todo!(),
        };

        match token {
            Token::Whitespace => {}
            _ => tokens.push(token),
        }
    }

    tokens
}
