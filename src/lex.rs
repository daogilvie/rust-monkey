use std::str::Chars;

#[derive(Debug, PartialEq)]
enum TokenType {
    ILLEGAL,
    EOF,
    // Identifiers & literals
    IDENT,
    INT,
    // Operators
    ASSIGN,
    PLUS,
    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    // Keywords
    FUNCTION,
    LET,
}

#[derive(Debug, PartialEq)]
struct Token {
    token_type: TokenType,
    literal: Option<String>,
}

impl Token {
    fn from_pair(token_type: TokenType, lit: String) -> Token {
        Token {
            token_type,
            literal: Some(lit),
        }
    }
}

struct Lexer<'a> {
    input_iter: Chars<'a>,
}

impl<'a> Lexer<'a> {
    fn for_str(input: &str) -> Lexer {
        Lexer {
            input_iter: input.chars(),
        }
    }

    fn for_string(input: &'a String) -> Lexer<'a> {
        Lexer {
            input_iter: input.chars(),
        }
    }

    fn next_token(&mut self) -> Token {
        match (self.read_char()) {
            None => Token {
                token_type: TokenType::EOF,
                literal: None,
            },
            Some(ch) => {
                let mut lit = String::with_capacity(1);
                lit.push(ch);
                let literal = Some(lit);
                match (ch) {
                    '=' => Token {
                        token_type: TokenType::ASSIGN,
                        literal,
                    },
                    '+' => Token {
                        token_type: TokenType::PLUS,
                        literal,
                    },
                    '(' => Token {
                        token_type: TokenType::LPAREN,
                        literal,
                    },
                    ')' => Token {
                        token_type: TokenType::RPAREN,
                        literal,
                    },
                    '{' => Token {
                        token_type: TokenType::LBRACE,
                        literal,
                    },
                    '}' => Token {
                        token_type: TokenType::RBRACE,
                        literal,
                    },
                    ',' => Token {
                        token_type: TokenType::COMMA,
                        literal,
                    },
                    ';' => Token {
                        token_type: TokenType::SEMICOLON,
                        literal,
                    },
                    _ => Token {
                        token_type: TokenType::ILLEGAL,
                        literal,
                    },
                }
            }
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input_iter.next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let mut lexer = Lexer::for_str("=+(){},;");

        let expected_tokens = [
            Token::from_pair(TokenType::ASSIGN, String::from("=")),
            Token::from_pair(TokenType::PLUS, String::from("+")),
            Token::from_pair(TokenType::LPAREN, String::from("(")),
            Token::from_pair(TokenType::RPAREN, String::from(")")),
            Token::from_pair(TokenType::LBRACE, String::from("{")),
            Token::from_pair(TokenType::RBRACE, String::from("}")),
            Token::from_pair(TokenType::COMMA, String::from(",")),
            Token::from_pair(TokenType::SEMICOLON, String::from(";")),
        ];

        for token in expected_tokens.iter() {
            let parsed = lexer.next_token();
            assert_eq!(&parsed, token);
        }
    }
}
