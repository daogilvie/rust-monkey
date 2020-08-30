use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Copy, Debug, PartialEq)]
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

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::ILLEGAL => f.write_str("ILLEGAL"),
            TokenType::EOF => f.write_str("EOF"),
            TokenType::IDENT => f.write_str("IDENT"),
            TokenType::INT => f.write_str("INT"),
            TokenType::ASSIGN => f.write_str("ASSIGN"),
            TokenType::PLUS => f.write_str("PLUS"),
            TokenType::MINUS => f.write_str("MINUS"),
            TokenType::BANG => f.write_str("BANG"),
            TokenType::ASTERISK => f.write_str("ASTERISK"),
            TokenType::SLASH => f.write_str("SLASH"),
            TokenType::LT => f.write_str("LT"),
            TokenType::GT => f.write_str("GT"),
            TokenType::EQ => f.write_str("EQ"),
            TokenType::NOTEQ => f.write_str("NOTEQ"),
            TokenType::COMMA => f.write_str("COMMA"),
            TokenType::SEMICOLON => f.write_str("SEMICOLON"),
            TokenType::LPAREN => f.write_str("LPAREN"),
            TokenType::RPAREN => f.write_str("RPAREN"),
            TokenType::LBRACE => f.write_str("LBRACE"),
            TokenType::RBRACE => f.write_str("RBRACE"),
            TokenType::FUNCTION => f.write_str("FUNCTION"),
            TokenType::LET => f.write_str("LET"),
            TokenType::IF => f.write_str("IF"),
            TokenType::ELSE => f.write_str("ELSE"),
            TokenType::RETURN => f.write_str("RETURN"),
            TokenType::TRUE => f.write_str("TRUE"),
            TokenType::FALSE => f.write_str("FALSE"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'literal> {
    pub token_type: TokenType,
    literal: Option<&'literal str>,
}

impl<'literal> fmt::Display for Token<'literal> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lit_string = match self.literal {
            None => "N/A",
            Some(s) => s,
        };
        write!(f, "{}(\"{}\")", self.token_type, lit_string)
    }
}

impl<'literal> Token<'literal> {
    pub fn with_str<'a>(token_type: TokenType, lit: &'a str) -> Token {
        Token {
            token_type,
            literal: Some(lit),
        }
    }

    pub fn get_literal(&self) -> Option<&str> {
        self.literal
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    input_iter: Peekable<Chars<'a>>,
    completed: bool,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn for_str(input: &str) -> Lexer {
        let mut input_iter = input.chars().peekable();
        let completed = input_iter.peek().is_none();
        Lexer {
            input,
            input_iter,
            completed,
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'a>, &'static str> {
        if self.completed {
            return Ok(Token {
                token_type: TokenType::EOF,
                literal: None,
            });
        }
        self.skip_whitespace();
        let start = self.position;

        match self.read_char() {
            None => {
                self.completed = true;
                Ok(Token {
                    token_type: TokenType::EOF,
                    literal: None,
                })
            }
            Some(ch) => {
                let likely_literal = self.input.get(start..self.position);
                match ch {
                    '=' => Ok(self.check_multichar_eq()),
                    '+' => Ok(Token {
                        token_type: TokenType::PLUS,
                        literal: likely_literal,
                    }),
                    '-' => Ok(Token {
                        token_type: TokenType::MINUS,
                        literal: likely_literal,
                    }),
                    '!' => Ok(self.check_multichar_bang()),
                    '*' => Ok(Token {
                        token_type: TokenType::ASTERISK,
                        literal: likely_literal,
                    }),
                    '/' => Ok(Token {
                        token_type: TokenType::SLASH,
                        literal: likely_literal,
                    }),
                    '<' => Ok(Token {
                        token_type: TokenType::LT,
                        literal: likely_literal,
                    }),
                    '>' => Ok(Token {
                        token_type: TokenType::GT,
                        literal: likely_literal,
                    }),
                    '(' => Ok(Token {
                        token_type: TokenType::LPAREN,
                        literal: likely_literal,
                    }),
                    ')' => Ok(Token {
                        token_type: TokenType::RPAREN,
                        literal: likely_literal,
                    }),
                    '{' => Ok(Token {
                        token_type: TokenType::LBRACE,
                        literal: likely_literal,
                    }),
                    '}' => Ok(Token {
                        token_type: TokenType::RBRACE,
                        literal: likely_literal,
                    }),
                    ',' => Ok(Token {
                        token_type: TokenType::COMMA,
                        literal: likely_literal,
                    }),
                    ';' => Ok(Token {
                        token_type: TokenType::SEMICOLON,
                        literal: likely_literal,
                    }),
                    l if l.is_alphabetic() => match self.read_ident(start) {
                        Some(ident) => Ok(Token {
                            token_type: Lexer::get_ident_tokentype(ident),
                            literal: Some(ident),
                        }),
                        None => Err("cannot read ident"),
                    },
                    d if d.is_digit(10) => Ok(Token {
                        token_type: TokenType::INT,
                        literal: self.read_int(start, 10),
                    }),
                    _ => Ok(Token {
                        token_type: TokenType::ILLEGAL,
                        literal: likely_literal,
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

    fn check_multichar_eq(&mut self) -> Token<'a> {
        let start = self.position - 1;
        let mut toktype = TokenType::ASSIGN;
        if let Some(c) = self.peek_char() {
            if *c == '=' {
                self.read_char();
                toktype = TokenType::EQ;
            }
        }
        Token {
            token_type: toktype,
            literal: self.input.get(start..self.position),
        }
    }

    fn check_multichar_bang(&mut self) -> Token<'a> {
        let start = self.position - 1;
        let mut toktype = TokenType::BANG;
        if let Some(c) = self.peek_char() {
            if *c == '=' {
                self.read_char();
                toktype = TokenType::NOTEQ;
            }
        }
        Token {
            token_type: toktype,
            literal: self.input.get(start..self.position),
        }
    }

    fn read_ident(&mut self, start: usize) -> Option<&'a str> {
        while let Some(&chr) = self.peek_char() {
            if chr.is_alphabetic() {
                self.read_char()?;
            } else {
                break;
            }
        }
        self.input.get(start..self.position)
    }

    fn get_ident_tokentype(ident: &str) -> TokenType {
        match ident {
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

    fn read_int(&mut self, start: usize, radix: u32) -> Option<&'a str> {
        while let Some(&chr) = self.peek_char() {
            if chr.is_digit(radix) {
                self.read_char()?;
            } else {
                break;
            }
        }
        self.input.get(start..self.position)
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input_iter.peek()
    }

    fn read_char(&mut self) -> Option<char> {
        let r = self.input_iter.next();
        self.position += 1;
        r
    }
}

impl<'literal> Iterator for Lexer<'literal> {
    type Item = Token<'literal>;

    fn next<'l>(&mut self) -> Option<Self::Item> {
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
                Err(err) => panic!("Unexpected error in lexing: {}", err),
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
