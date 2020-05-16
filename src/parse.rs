use std::mem;

#[path = "./lex.rs"]
mod lex;

use lex::TokenType::*;

mod ast {
    use super::lex;
    pub enum ExpressionNode {
        Stub,
        Identifier { token: lex::Token },
    }

    pub enum StatementNode {
        Let {
            token: lex::Token,
            name: ExpressionNode,
            value: ExpressionNode,
        },
        Return {
            token: lex::Token,
            value: ExpressionNode,
        },
    }

    /// Root node of all Monkey ASTs
    pub struct Program {
        pub statements: Vec<StatementNode>,
    }
}

struct Parser<'a> {
    lexer: lex::Lexer<'a>,
    current_token: lex::Token,
    peek_token: Option<lex::Token>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn for_lexer(mut lexer: lex::Lexer) -> Parser {
        let first_token = lexer.next_token().expect("Passed an empty lexer");
        let second_token = lexer.next_token().expect("Passed an empty lexer");

        Parser {
            lexer,
            current_token: first_token,
            peek_token: Some(second_token),
            errors: Vec::new(),
        }
    }

    pub fn advance_tokens(&mut self) -> lex::Token {
        let next = self.lexer.next_token().expect("Unable to advance token");
        let mut peek_unwrapped: lex::Token = self.peek_token.take().unwrap();
        mem::swap(&mut peek_unwrapped, &mut self.current_token);
        self.peek_token = Some(next);
        peek_unwrapped
    }

    pub fn parse(&mut self) -> Result<ast::Program, String> {
        let mut program = ast::Program {
            statements: Vec::new(),
        };
        while !self.is_current_token_of_type(EOF) {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => self.errors.push(String::from(err)),
            }
            self.advance_tokens();
        }
        if self.errors.is_empty() {
            Ok(program)
        } else {
            let err_count = self.errors.len();
            let mut plural = "s";
            if err_count == 1 {
                plural = "";
            }
            let err_msg = format!("Parser failed with {} error{}", err_count, plural);
            Err(err_msg)
        }
    }

    fn parse_statement(&mut self) -> Result<ast::StatementNode, String> {
        match self.current_token.token_type {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => Err(format!(
                "No known statement starts with {}",
                &self.current_token
            )),
        }
    }

    fn is_next_token_of_type(&self, expected_type: lex::TokenType) -> bool {
        match &self.peek_token {
            None => false,
            Some(t) => t.token_type == expected_type,
        }
    }

    fn is_current_token_of_type(&self, expected_type: lex::TokenType) -> bool {
        self.current_token.token_type == expected_type
    }

    fn parse_let_statement(&mut self) -> Result<ast::StatementNode, String> {
        if !self.is_next_token_of_type(IDENT) {
            return Err(format!(
                "Expecting assign token after ident, got {}",
                match &self.peek_token {
                    None => String::from("None"),
                    Some(t) => format!("{}", t),
                }
            ));
        }
        let original_token = self.advance_tokens();
        if !self.is_next_token_of_type(ASSIGN) {
            return Err(format!(
                "Expecting assign token after ident, got {}",
                match &self.peek_token {
                    None => String::from("None"),
                    Some(t) => format!("{}", t),
                }
            ));
        }
        let ident_token = self.advance_tokens();
        // TODO: We would do the value, but instead we will skip for now
        while self.current_token.token_type != SEMICOLON {
            self.advance_tokens();
        }
        let name = ast::ExpressionNode::Identifier { token: ident_token };
        let value = ast::ExpressionNode::Stub;
        Ok(ast::StatementNode::Let {
            token: original_token,
            name,
            value,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ast::StatementNode, String> {
        let original_token = self.advance_tokens();
        // TODO: We would do the value, but instead we will skip for now
        while self.current_token.token_type != SEMICOLON {
            self.advance_tokens();
        }
        let value = ast::ExpressionNode::Stub;
        Ok(ast::StatementNode::Return {
            token: original_token,
            value,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_next_token_init() {
        let lexer = lex::Lexer::for_str("let x = 10;");
        let parser = Parser::for_lexer(lexer);
        assert_eq!(parser.current_token.get_literal().unwrap(), "let");
        assert_eq!(parser.peek_token.unwrap().get_literal().unwrap(), "x");
    }

    #[test]
    fn test_parse_let_statements() {
        let input = "let x = 5;

        let y = 10;

        let foobar = 838383;
        ";
        let lexer = lex::Lexer::for_str(input);
        let mut parser = Parser::for_lexer(lexer);
        let parsed = parser.parse();
        assert_ne!(
            parsed.is_err(),
            true,
            "Parser failed with error \"{}\"",
            parsed.err().unwrap()
        );
        let program = parsed.unwrap();
        let length = program.statements.len();
        assert_eq!(
            length, 3,
            "Expecting program to contain 3 statements, got {}",
            length
        );
        let expected_idents = vec![("x", "5"), ("y", "10"), ("foobar", "838383")];
        let pairs = program.statements.iter().zip(expected_idents);
        for (statement, ident) in pairs {
            match statement {
                ast::StatementNode::Let {
                    token: _,
                    name,
                    value: _,
                } => match name {
                    ast::ExpressionNode::Identifier { token } => {
                        assert_eq!(token.get_literal().unwrap(), ident.0);
                    }
                    _ => assert!(false, "Encountered an unexpected non-identifier name"),
                },
                _ => assert!(false, "Encountered unexpected non-let statement"),
            }
        }
    }

    #[test]
    fn test_parse_let_statements_errors() {
        let input = "let x 5;

        let = 10;

        let 838383;
        ";
        let lexer = lex::Lexer::for_str(input);
        let mut parser = Parser::for_lexer(lexer);
        let parsed = parser.parse();
        assert_eq!(parsed.is_err(), true);
        let err = parsed.err().unwrap();
        assert_eq!(err, "Parser failed with 10 errors");

        assert_eq!(parser.errors.len(), 10);

        let expected_errors = vec![
            "Expecting assign token after ident, got INT(\"5\")",
            "No known statement starts with INT(\"5\")",
            "No known statement starts with SEMICOLON(\";\")",
            "Expecting assign token after ident, got ASSIGN(\"=\")",
            "No known statement starts with ASSIGN(\"=\")",
            "No known statement starts with INT(\"10\")",
            "No known statement starts with SEMICOLON(\";\")",
            "Expecting assign token after ident, got INT(\"838383\")",
            "No known statement starts with INT(\"838383\")",
            "No known statement starts with SEMICOLON(\";\")",
        ];
        for (actual, expected) in parser.errors.iter().zip(expected_errors) {
            assert_eq!(
                actual, expected,
                "Error did not match expectation: wanted {}, got {}",
                expected, actual
            );
        }
    }

    #[test]
    fn test_parse_return_statements() {
        let input = "return 5;

        return 10;

        return 838383;
        ";
        let lexer = lex::Lexer::for_str(input);
        let mut parser = Parser::for_lexer(lexer);
        let parsed = parser.parse();
        assert_ne!(
            parsed.is_err(),
            true,
            "Parser failed with error \"{}\"",
            parsed.err().unwrap()
        );
        let program = parsed.unwrap();
        let length = program.statements.len();
        assert_eq!(
            length, 3,
            "Expecting program to contain 3 statements, got {}",
            length
        );
        for statement in program.statements {
            match statement {
                ast::StatementNode::Return { token, value: _ } => {
                    assert_eq!(token.get_literal().unwrap(), "return", "Return statement has non-return token");
                },
                _ => assert!(false, "Encountered unexpected non-return statement"),
            }
        }
    }
}
