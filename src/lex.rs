use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    // Identifiers & literals
    IDENT,
    INT,
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOTEQ,
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
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    literal: Option<String>,
}

impl Token {
    fn with_str(token_type: TokenType, lit: &str) -> Token {
        Token {
            token_type,
            literal: Some(String::from(lit)),
        }
    }
}

pub struct Lexer<'a> {
    input_iter: Peekable<Chars<'a>>,
    completed: bool,
}

impl<'a> Lexer<'a> {

    pub fn for_str(input: &str) -> Lexer {
        Lexer {
            input_iter: input.chars().peekable(),
            completed: false,
        }
    }

    fn next_token(&mut self) -> Result<Token, &'static str> {
        self.skip_whitespace();

        match self.read_char() {
            None => Ok(Token {
                token_type: TokenType::EOF,
                literal: None,
            }),
            Some(ch) => {
                let mut lit = String::with_capacity(1);
                lit.push(ch);
                let literal = Some(lit);
                match ch {
                    '=' => Ok(self.check_multichar_eq(literal)),
                    '+' => Ok(Token {
                        token_type: TokenType::PLUS,
                        literal,
                    }),
                    '-' => Ok(Token {
                        token_type: TokenType::MINUS,
                        literal,
                    }),
                    '!' => Ok(self.check_multichar_bang(literal)),
                    '*' => Ok(Token {
                        token_type: TokenType::ASTERISK,
                        literal,
                    }),
                    '/' => Ok(Token {
                        token_type: TokenType::SLASH,
                        literal,
                    }),
                    '<' => Ok(Token {
                        token_type: TokenType::LT,
                        literal,
                    }),
                    '>' => Ok(Token {
                        token_type: TokenType::GT,
                        literal,
                    }),
                    '(' => Ok(Token {
                        token_type: TokenType::LPAREN,
                        literal,
                    }),
                    ')' => Ok(Token {
                        token_type: TokenType::RPAREN,
                        literal,
                    }),
                    '{' => Ok(Token {
                        token_type: TokenType::LBRACE,
                        literal,
                    }),
                    '}' => Ok(Token {
                        token_type: TokenType::RBRACE,
                        literal,
                    }),
                    ',' => Ok(Token {
                        token_type: TokenType::COMMA,
                        literal,
                    }),
                    ';' => Ok(Token {
                        token_type: TokenType::SEMICOLON,
                        literal,
                    }),
                    l if l.is_alphabetic() => match self.read_ident(ch) {
                        Some(ident) => Ok(Token {
                            token_type: Lexer::get_ident_tokentype(&ident),
                            literal: Some(ident),
                        }),
                        None => Err("cannot read ident"),
                    },
                    d if d.is_digit(10) => Ok(Token {
                        token_type: TokenType::INT,
                        literal: self.read_int(ch, 10),
                    }),
                    _ => Ok(Token {
                        token_type: TokenType::ILLEGAL,
                        literal,
                    }),
                }
            }
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let next_char = self.peek_char();
            match next_char {
                None => break,
                Some(c) => match c {
                    w if w.is_whitespace() => {
                        self.read_char();
                    }
                    _ => break,
                },
            }
        }
    }

    fn check_multichar_eq(&mut self, literal: Option<String>) -> Token {
        let next_char = self.peek_char();
        match next_char {
            Some(c) => match c {
                '=' => {
                    self.read_char();
                    Token::with_str(TokenType::EQ, "==")
                }
                _ => Token {
                    token_type: TokenType::ASSIGN,
                    literal,
                },
            },
            _ => Token {
                token_type: TokenType::ASSIGN,
                literal,
            },
        }
    }

    fn check_multichar_bang(&mut self, literal: Option<String>) -> Token {
        let next_char = self.peek_char();
        match next_char {
            Some(c) => match c {
                '=' => {
                    self.read_char();
                    Token::with_str(TokenType::NOTEQ, "!=")
                }
                _ => Token {
                    token_type: TokenType::BANG,
                    literal,
                },
            },
            _ => Token {
                token_type: TokenType::BANG,
                literal,
            },
        }
    }

    fn read_ident(&mut self, prefix: char) -> Option<String> {
        let mut ident = String::from("");
        ident.push(prefix);
        while let Some(&chr) = self.peek_char() {
            if chr.is_alphabetic() {
                ident.push(self.read_char()?);
            } else {
                break;
            }
        }
        Some(ident)
    }

    fn get_ident_tokentype(ident: &String) -> TokenType {
        match ident.as_str() {
            "let" => TokenType::LET,
            "fn" => TokenType::FUNCTION,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            _ => TokenType::IDENT,
        }
    }

    fn read_int(&mut self, prefix: char, radix: u32) -> Option<String> {
        let mut ident = String::from("");
        ident.push(prefix);
        while let Some(&chr) = self.peek_char() {
            if chr.is_digit(radix) {
                ident.push(self.read_char()?);
            } else {
                break;
            }
        }
        Some(ident)
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input_iter.peek()
    }

    fn read_char(&mut self) -> Option<char> {
        self.input_iter.next()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.completed {
            None
        } else {
            match self.next_token() {
                Ok(token) => {
                    if token.token_type == TokenType::EOF {
                        self.completed = true;
                    }
                    Some(token)
                }
                Err(err) => panic!("Unexpected error in lexing: {}", err)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;

    #[test]
    fn test_next_token() {
        let mut lexer = Lexer::for_str("=+(){},;");

        let expected_tokens = [
            Token::with_str(ASSIGN, "="),
            Token::with_str(PLUS, "+"),
            Token::with_str(LPAREN, "("),
            Token::with_str(RPAREN, ")"),
            Token::with_str(LBRACE, "{"),
            Token::with_str(RBRACE, "}"),
            Token::with_str(COMMA, ","),
            Token::with_str(SEMICOLON, ";"),
        ];

        for token in expected_tokens.iter() {
            let parsed = lexer.next_token().unwrap();
            assert_eq!(&parsed, token);
        }
    }

    #[test]
    fn test_lex_simple_script() {
        let input = "let five = 5;
        
        let ten = 10;

        
        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);

        ";
        let mut lexer = Lexer::for_str(input);
        let expected_tokens = [
            Token::with_str(LET, "let"),
            Token::with_str(IDENT, "five"),
            Token::with_str(ASSIGN, "="),
            Token::with_str(INT, "5"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(LET, "let"),
            Token::with_str(IDENT, "ten"),
            Token::with_str(ASSIGN, "="),
            Token::with_str(INT, "10"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(LET, "let"),
            Token::with_str(IDENT, "add"),
            Token::with_str(ASSIGN, "="),
            Token::with_str(FUNCTION, "fn"),
            Token::with_str(LPAREN, "("),
            Token::with_str(IDENT, "x"),
            Token::with_str(COMMA, ","),
            Token::with_str(IDENT, "y"),
            Token::with_str(RPAREN, ")"),
            Token::with_str(LBRACE, "{"),
            Token::with_str(IDENT, "x"),
            Token::with_str(PLUS, "+"),
            Token::with_str(IDENT, "y"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(RBRACE, "}"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(LET, "let"),
            Token::with_str(IDENT, "result"),
            Token::with_str(ASSIGN, "="),
            Token::with_str(IDENT, "add"),
            Token::with_str(LPAREN, "("),
            Token::with_str(IDENT, "five"),
            Token::with_str(COMMA, ","),
            Token::with_str(IDENT, "ten"),
            Token::with_str(RPAREN, ")"),
            Token::with_str(SEMICOLON, ";"),
            Token {
                token_type: EOF,
                literal: None,
            },
        ];

        for token in expected_tokens.iter() {
            let parsed = lexer.next_token().unwrap();
            assert_eq!(&parsed, token);
        }
    }

    #[test]
    fn test_lex_script_extended_set() {
        let input = "let five = 5;
        
        let ten = 10;

        
        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);

        !-/*5;

        5 < 10 > 5;

        if (5 < 10) {

          return true;

        } else {
          return false;
        }

        10 == 10;
        10 != 9;
        ";
        let mut lexer = Lexer::for_str(input);
        let expected_tokens = [
            Token::with_str(LET, "let"),
            Token::with_str(IDENT, "five"),
            Token::with_str(ASSIGN, "="),
            Token::with_str(INT, "5"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(LET, "let"),
            Token::with_str(IDENT, "ten"),
            Token::with_str(ASSIGN, "="),
            Token::with_str(INT, "10"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(LET, "let"),
            Token::with_str(IDENT, "add"),
            Token::with_str(ASSIGN, "="),
            Token::with_str(FUNCTION, "fn"),
            Token::with_str(LPAREN, "("),
            Token::with_str(IDENT, "x"),
            Token::with_str(COMMA, ","),
            Token::with_str(IDENT, "y"),
            Token::with_str(RPAREN, ")"),
            Token::with_str(LBRACE, "{"),
            Token::with_str(IDENT, "x"),
            Token::with_str(PLUS, "+"),
            Token::with_str(IDENT, "y"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(RBRACE, "}"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(LET, "let"),
            Token::with_str(IDENT, "result"),
            Token::with_str(ASSIGN, "="),
            Token::with_str(IDENT, "add"),
            Token::with_str(LPAREN, "("),
            Token::with_str(IDENT, "five"),
            Token::with_str(COMMA, ","),
            Token::with_str(IDENT, "ten"),
            Token::with_str(RPAREN, ")"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(BANG, "!"),
            Token::with_str(MINUS, "-"),
            Token::with_str(SLASH, "/"),
            Token::with_str(ASTERISK, "*"),
            Token::with_str(INT, "5"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(INT, "5"),
            Token::with_str(LT, "<"),
            Token::with_str(INT, "10"),
            Token::with_str(GT, ">"),
            Token::with_str(INT, "5"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(IF, "if"),
            Token::with_str(LPAREN, "("),
            Token::with_str(INT, "5"),
            Token::with_str(LT, "<"),
            Token::with_str(INT, "10"),
            Token::with_str(RPAREN, ")"),
            Token::with_str(LBRACE, "{"),
            Token::with_str(RETURN, "return"),
            Token::with_str(TRUE, "true"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(RBRACE, "}"),
            Token::with_str(ELSE, "else"),
            Token::with_str(LBRACE, "{"),
            Token::with_str(RETURN, "return"),
            Token::with_str(FALSE, "false"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(RBRACE, "}"),
            Token::with_str(INT, "10"),
            Token::with_str(EQ, "=="),
            Token::with_str(INT, "10"),
            Token::with_str(SEMICOLON, ";"),
            Token::with_str(INT, "10"),
            Token::with_str(NOTEQ, "!="),
            Token::with_str(INT, "9"),
            Token::with_str(SEMICOLON, ";"),
            Token {
                token_type: EOF,
                literal: None,
            },
        ];

        for token in expected_tokens.iter() {
            let parsed = lexer.next_token().unwrap();
            assert_eq!(&parsed, token);
        }
    }
}
