mod propositional;
pub use propositional::parse_propositional;

#[derive(Clone, Debug)]
enum Token {
    BraceL,
    BraceR,
    BracketL,
    BracketR,
    ParenL,
    ParenR,
    Constant(String),
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

            char if char.is_ascii_alphanumeric() && char.is_lowercase() => {
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
                    _ => tokens.push(Token::Constant(string)),
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
