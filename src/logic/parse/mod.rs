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
}

type TokenVec = Vec<Token>;

fn lex(expr: &str) -> TokenVec {
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
            '.' => tokens.push(Token::Stop),

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

                let token = match string.as_str() {
                    "true" => Token::True,
                    "false" => Token::False,
                    "not" => Token::Not,
                    "and" => Token::And,
                    "or" => Token::Or,
                    "implies" => Token::Imp,
                    "iff" => Token::Iff,
                    "forall" => Token::Quantifier(Quantifier::ForAll),
                    "exists" => Token::Quantifier(Quantifier::Exists),
                    _ => Token::Identifier(string),
                };

                tokens.push(token)
            }

            whitespace if whitespace.is_whitespace() => {}
            _ => {
                todo!()
            }
        }
    }

    tokens
}
