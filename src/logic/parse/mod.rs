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
}

type TokenVec = Vec<Token>;

fn lex(expr: &str) -> Vec<Token> {
    let mut tokens = Vec::default();

    let mut chars = expr.chars().peekable();

    while let Some(char) = chars.next() {
        match char {
            '(' => tokens.push(Token::ParenL(Paren::Blinky)),
            ')' => tokens.push(Token::ParenR(Paren::Blinky)),
            '[' => tokens.push(Token::ParenL(Paren::Pinky)),
            ']' => tokens.push(Token::ParenR(Paren::Pinky)),
            '{' => tokens.push(Token::ParenL(Paren::Inky)),
            '}' => tokens.push(Token::ParenR(Paren::Inky)),

            'T' => tokens.push(Token::True),
            'F' => tokens.push(Token::False),

            '~' => tokens.push(Token::Not),

            '|' => tokens.push(Token::Or),
            '&' => tokens.push(Token::And),

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

            ',' => tokens.push(Token::Comma),

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
                    _ => tokens.push(Token::Identifier(string)),
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
