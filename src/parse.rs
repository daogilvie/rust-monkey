use std::mem;

#[path = "./lex.rs"]
mod lex;

use lex::TokenType::*;

mod ast {
    use super::lex;
    use std::fmt;

    pub trait ASTNode {
        fn get_token_literal(&self) -> Option<&String>;
    }

    #[derive(Debug)]
    pub enum ExpressionKind {
        Stub,
        Identifier,
        IntegerLiteral {
            value: i64,
        },
        PrefixExpression {
            operator: lex::Token,
            right: Box<ExpressionNode>,
        },
        InfixExpression {
            left: Box<ExpressionNode>,
            operator: lex::Token,
            right: Box<ExpressionNode>,
        },
    }

    #[derive(Debug)]
    pub struct ExpressionNode {
        pub token: lex::Token,
        pub kind: ExpressionKind,
    }

    impl ExpressionNode {
        pub fn make_stub() -> ExpressionNode {
            ExpressionNode {
                token: lex::Token::with_str(lex::TokenType::ILLEGAL, "<STUB>"),
                kind: ExpressionKind::Stub,
            }
        }
    }

    impl ASTNode for ExpressionNode {
        fn get_token_literal(&self) -> Option<&String> {
            self.token.get_literal()
        }
    }

    impl fmt::Display for ExpressionNode {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match &self.kind {
                ExpressionKind::Stub => f.write_str("<STUB>"),
                ExpressionKind::Identifier => write!(f, "{}", self.get_token_literal().unwrap()),
                ExpressionKind::IntegerLiteral { value } => write!(f, "{}", value),
                ExpressionKind::PrefixExpression { operator, right } => {
                    write!(f, "({}{})", operator.get_literal().unwrap(), right)
                }
                ExpressionKind::InfixExpression {
                    left,
                    operator,
                    right,
                } => write!(
                    f,
                    "({} {} {})",
                    left,
                    operator.get_literal().unwrap(),
                    right
                ),
            }
        }
    }

    #[derive(Debug)]
    pub enum StatementKind {
        Let {
            name: ExpressionNode,
            value: ExpressionNode,
        },
        Return {
            value: ExpressionNode,
        },
        Expression {
            expression: ExpressionNode,
        },
    }

    #[derive(Debug)]
    pub struct StatementNode {
        pub token: lex::Token,
        pub kind: StatementKind,
    }

    impl ASTNode for StatementNode {
        fn get_token_literal(&self) -> Option<&String> {
            self.token.get_literal()
        }
    }

    impl fmt::Display for StatementNode {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match &self.kind {
                StatementKind::Let { name, value } => write!(
                    f,
                    "{} {} = {};",
                    self.get_token_literal().unwrap(),
                    name,
                    value
                ),
                StatementKind::Return { value } => {
                    write!(f, "{} {};", self.get_token_literal().unwrap(), value)
                }
                StatementKind::Expression { expression } => write!(f, "{}", expression),
            }
        }
    }

    /// Root node of all Monkey ASTs
    pub struct Program {
        pub statements: Vec<StatementNode>,
    }

    impl fmt::Display for Program {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            for statement in &self.statements {
                writeln!(f, "{}", statement)?;
            }
            f.write_str("")
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;
        #[test]
        fn test_display_impl() {
            let prog = Program {
                statements: vec![StatementNode {
                    token: lex::Token::with_str(lex::TokenType::LET, "let"),
                    kind: StatementKind::Let {
                        name: ExpressionNode {
                            token: lex::Token::with_str(lex::TokenType::IDENT, "myVar"),
                            kind: ExpressionKind::Identifier,
                        },
                        value: ExpressionNode {
                            token: lex::Token::with_str(lex::TokenType::IDENT, "anotherVar"),
                            kind: ExpressionKind::Identifier,
                        },
                    },
                }],
            };

            let prog_string = format!("{}", prog);
            assert_eq!(prog_string, "let myVar = anotherVar;\n");
        }
    }
}

use ast::*;

#[derive(Debug, PartialEq, PartialOrd)]
enum OperatorPrecedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

fn get_operator_precedence(&token: &lex::TokenType) -> OperatorPrecedence {
    match &token {
        lex::TokenType::EQ => OperatorPrecedence::EQUALS,
        lex::TokenType::NOTEQ => OperatorPrecedence::EQUALS,
        lex::TokenType::LT => OperatorPrecedence::LESSGREATER,
        lex::TokenType::GT => OperatorPrecedence::LESSGREATER,
        lex::TokenType::PLUS => OperatorPrecedence::SUM,
        lex::TokenType::MINUS => OperatorPrecedence::SUM,
        lex::TokenType::SLASH => OperatorPrecedence::PRODUCT,
        lex::TokenType::ASTERISK => OperatorPrecedence::PRODUCT,
        _ => OperatorPrecedence::LOWEST,
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

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut program = Program {
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

    fn parse_statement(&mut self) -> Result<StatementNode, String> {
        match self.current_token.token_type {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => self.attempt_parse_expression_statement(),
        }
    }

    fn is_next_token_of_type(&self, expected_type: lex::TokenType) -> bool {
        match &self.peek_token {
            None => false,
            Some(t) => t.token_type == expected_type,
        }
    }

    fn get_next_token_precedence(&self) -> OperatorPrecedence {
        match &self.peek_token {
            None => OperatorPrecedence::LOWEST,
            Some(t) => get_operator_precedence(&t.token_type),
        }
    }

    fn get_current_token_precedence(&self) -> OperatorPrecedence {
        get_operator_precedence(&self.current_token.token_type)
    }

    fn is_current_token_of_type(&self, expected_type: lex::TokenType) -> bool {
        self.current_token.token_type == expected_type
    }

    fn parse_let_statement(&mut self) -> Result<StatementNode, String> {
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
        let name = ExpressionNode {
            token: ident_token,
            kind: ExpressionKind::Identifier,
        };
        let value = ExpressionNode::make_stub();
        Ok(StatementNode {
            token: original_token,
            kind: StatementKind::Let { name, value },
        })
    }

    fn parse_return_statement(&mut self) -> Result<StatementNode, String> {
        let original_token = self.advance_tokens();
        // TODO: We would do the value, but instead we will skip for now
        while self.current_token.token_type != SEMICOLON {
            self.advance_tokens();
        }
        let value = ExpressionNode::make_stub();
        Ok(StatementNode {
            token: original_token,
            kind: StatementKind::Return { value },
        })
    }

    fn attempt_parse_expression_statement(&mut self) -> Result<StatementNode, String> {
        let original_token = self.current_token.clone();
        let result = self.parse_expression(OperatorPrecedence::LOWEST)?;

        match &self.peek_token {
            Some(token) => {
                if token.token_type == SEMICOLON {
                    self.advance_tokens();
                }
            }
            None => {}
        }

        Ok(StatementNode {
            token: original_token,
            kind: StatementKind::Expression { expression: result },
        })
    }

    fn parse_expression(
        &mut self,
        precedence: OperatorPrecedence,
    ) -> Result<ExpressionNode, String> {
        // This is the power-house of the Pratt-esque parsing
        // strategy that the book gets us to implement.
        // The key tactic is that we attempt to parse the expression as a prefix
        // if appropriate. If that succeeds, we see if this is just the LHS of
        // an infix expression, and we iterate over this procedure until we reach
        // the end of the expression.
        let prefix = self.attempt_prefix_parse()?;
        match prefix {
            Some(expr) => {
                let mut current_expr = expr;
                while !self.is_next_token_of_type(lex::TokenType::SEMICOLON)
                    && precedence < self.get_next_token_precedence()
                    && self.can_parse_infix()
                {
                    self.advance_tokens();
                    current_expr = self.parse_infix_expression(current_expr)?;
                }
                Ok(current_expr)
            }
            None => Err(format!(
                "Cannot parse expression statement from {}",
                self.current_token
            )),
        }
    }

    fn attempt_prefix_parse(&mut self) -> Result<Option<ExpressionNode>, String> {
        let parse_attempt = match &self.current_token.token_type {
            lex::TokenType::IDENT => self.parse_identifier()?,
            lex::TokenType::INT => self.parse_integer_literal()?,
            lex::TokenType::BANG => self.parse_prefix_expression()?,
            lex::TokenType::MINUS => self.parse_prefix_expression()?,
            _ => return Ok(None),
        };
        Ok(Some(parse_attempt))
    }

    fn can_parse_infix(&self) -> bool {
        match &self.peek_token {
            None => false,
            Some(t) => match &t.token_type {
                lex::TokenType::MINUS => true,
                lex::TokenType::PLUS => true,
                lex::TokenType::EQ => true,
                lex::TokenType::NOTEQ => true,
                lex::TokenType::GT => true,
                lex::TokenType::LT => true,
                lex::TokenType::ASTERISK => true,
                lex::TokenType::SLASH => true,
                _ => false,
            },
        }
    }

    fn parse_identifier(&mut self) -> Result<ExpressionNode, String> {
        Ok(ExpressionNode {
            token: self.current_token.clone(),
            kind: ExpressionKind::Identifier,
        })
    }

    fn parse_integer_literal(&mut self) -> Result<ExpressionNode, String> {
        let token = self.current_token.clone();
        let lit = token.get_literal().unwrap();
        let attempted_parse = lit.parse();
        match attempted_parse {
            Ok(value) => Ok(ExpressionNode {
                token,
                kind: ExpressionKind::IntegerLiteral { value },
            }),
            Err(_err) => Err(format!("Cannot parse '{}' to i64", lit)),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<ExpressionNode, String> {
        let operator_token = self.advance_tokens();

        let rhs = self.parse_expression(OperatorPrecedence::PREFIX)?;
        Ok(ExpressionNode {
            token: operator_token.clone(),
            kind: ExpressionKind::PrefixExpression {
                operator: operator_token,
                right: Box::new(rhs),
            },
        })
    }

    fn parse_infix_expression(&mut self, lhs: ExpressionNode) -> Result<ExpressionNode, String> {
        let current_precedence = self.get_current_token_precedence();
        let expr_token = self.advance_tokens();
        let rhs = self.parse_expression(current_precedence)?;

        Ok(ExpressionNode {
            token: expr_token.clone(),
            kind: ExpressionKind::InfixExpression {
                left: Box::new(lhs),
                operator: expr_token,
                right: Box::new(rhs),
            },
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
            match &statement.kind {
                StatementKind::Let { name, value: _ } => match name.kind {
                    ExpressionKind::Identifier => {
                        assert_eq!(name.get_token_literal().unwrap(), ident.0);
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
        assert_eq!(err, "Parser failed with 4 errors");

        assert_eq!(parser.errors.len(), 4);

        let expected_errors = vec![
            "Expecting assign token after ident, got INT(\"5\")",
            "Expecting assign token after ident, got ASSIGN(\"=\")",
            "Cannot parse expression statement from ASSIGN(\"=\")",
            "Expecting assign token after ident, got INT(\"838383\")",
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
            match statement.kind {
                StatementKind::Return { value: _ } => {
                    assert_eq!(
                        statement.get_token_literal().unwrap(),
                        "return",
                        "Return statement has non-return token"
                    );
                }
                _ => assert!(false, "Encountered unexpected non-return statement"),
            }
        }
    }

    #[test]
    fn test_parse_identifier_expression() {
        let input = "foobar;";
        let lexer = lex::Lexer::for_str(input);
        let mut parser = Parser::for_lexer(lexer);
        let parsed = parser.parse();
        assert_ne!(
            parsed.is_err(),
            true,
            "Parser failed with errors \"{:?}\"",
            parser.errors
        );
        let program = parsed.unwrap();
        let length = program.statements.len();
        assert_eq!(
            length, 1,
            "Expecting program to contain 1 statement, got {}",
            length
        );
        let statement = program.statements.get(0).unwrap();
        match &statement.kind {
            StatementKind::Expression { expression } => match &expression.kind {
                ExpressionKind::Identifier => {
                    assert_eq!(expression.get_token_literal().unwrap(), "foobar");
                }
                _ => {
                    assert!(false, "Statement expression is not identifier");
                }
            },
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    #[test]
    fn test_parse_int_literal_expression() {
        let input = "5;";
        let lexer = lex::Lexer::for_str(input);
        let mut parser = Parser::for_lexer(lexer);
        let parsed = parser.parse();
        assert_ne!(
            parsed.is_err(),
            true,
            "Parser failed with errors \"{:?}\"",
            parser.errors
        );
        let program = parsed.unwrap();
        let length = program.statements.len();
        assert_eq!(
            length, 1,
            "Expecting program to contain 1 statement, got {}",
            length
        );
        let statement = program.statements.get(0).unwrap();
        match &statement.kind {
            StatementKind::Expression { expression } => match &expression.kind {
                ExpressionKind::IntegerLiteral { value } => {
                    assert_eq!(
                        expression.get_token_literal().unwrap(),
                        "5",
                        "expecting literal string '5'"
                    );
                    assert_eq!(*value, 5, "Expecting calculated value 5");
                }
                _ => {
                    assert!(false, "Statement expression is not integer literal");
                }
            },
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    mod test_prefix_funcs {
        use super::*;
        macro_rules! prefix_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_operator, expected_value) = $value;
                    let lexer = lex::Lexer::for_str(input);
                    let mut parser = Parser::for_lexer(lexer);
                    let parsed = parser.parse();
                    assert_ne!(
                        parsed.is_err(),
                        true,
                        "Parser failed with errors \"{:?}\"",
                        parser.errors
                    );
                    let program = parsed.unwrap();
                    let length = program.statements.len();
                    assert_eq!(
                        length, 1,
                        "Expecting program to contain 1 statement, got {}",
                        length
                    );
                    let statement = program.statements.get(0).unwrap();

                    match &statement.kind {
                        StatementKind::Expression { expression } => match &expression.kind {
                            ExpressionKind::PrefixExpression { operator, right } => {
                                assert_eq!(
                                    operator.get_literal().unwrap(),
                                    expected_operator
                                );
                                let deboxed = &*right;
                                match deboxed.kind {
                                    ExpressionKind::IntegerLiteral { value } => {
                                        assert_eq!(value, expected_value)
                                    },
                                    _ => assert!(false, "Prefix-op right is not an int literal")
                                }
                            }
                            _ => {
                                assert!(false, "Statement expression is not prefix op expression");
                            }
                        },
                        _ => assert!(false, "Encountered unexpected non-expression statement"),
                    }
                })*

        }
    }

        prefix_tests! {
            notfive: ("!5", "!", 5),
            negfifteen: ("-15", "-", 15),
        }
    }

    mod test_infix_funcs {
        use super::*;
        macro_rules! infix_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_lhs, expected_operator, expected_rhs) = $value;
                    let lexer = lex::Lexer::for_str(input);
                    let mut parser = Parser::for_lexer(lexer);
                    let parsed = parser.parse();
                    assert_ne!(
                        parsed.is_err(),
                        true,
                        "Parser failed with errors \"{:?}\"",
                        parser.errors
                    );
                    let program = parsed.unwrap();
                    let length = program.statements.len();
                    assert_eq!(
                        length, 1,
                        "Expecting program to contain 1 statement, got {:?}",
                        program.statements
                    );
                    let statement = program.statements.get(0).unwrap();

                    match &statement.kind {
                        StatementKind::Expression { expression } => match &expression.kind {
                            ExpressionKind::InfixExpression { left, operator, right } => {
                                assert_eq!(
                                    operator.get_literal().unwrap(),
                                    expected_operator
                                );
                                let deboxed_left = &*left;
                                match deboxed_left.kind {
                                    ExpressionKind::IntegerLiteral { value } => {
                                        assert_eq!(value, expected_lhs)
                                    },
                                    _ => assert!(false, "left is not an int literal")
                                }
                                let deboxed_right = &*right;
                                match deboxed_right.kind {
                                    ExpressionKind::IntegerLiteral { value } => {
                                        assert_eq!(value, expected_rhs)
                                    },
                                    _ => assert!(false, "right is not an int literal")
                                }
                            }
                            _ => {
                                assert!(false, "Statement expression is not infix op expression");
                            }
                        },
                        _ => assert!(false, "Encountered unexpected non-expression statement"),
                    }
                })*

        }
    }

        infix_tests! {
            plus: ("5 + 5;", 5, "+", 5),
            minus: ("5 - 5;", 5, "-", 5),
            asterisk: ("5 * 5", 5, "*", 5),
            slash: ("5 / 5", 5, "/", 5),
            gt: ("5 > 5", 5, ">", 5),
            lt: ("5 < 5", 5, "<", 5),
            eqeq: ("5 == 5", 5, "==", 5),
            noteq: ("5 != 5", 5, "!=", 5),
        }
    }

    mod test_precedence_parsing {
        use super::*;
        macro_rules! precedence_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_repr) = $value;
                    let lexer = lex::Lexer::for_str(input);
                    let mut parser = Parser::for_lexer(lexer);
                    let parsed = parser.parse();
                    assert_ne!(
                        parsed.is_err(),
                        true,
                        "Parser failed with errors \"{:?}\"",
                        parser.errors
                    );
                    let program = parsed.unwrap();
                    let statements = program.statements.into_iter().map(|s| format!("{}", s)).collect::<String>();
                    assert_eq!(statements, expected_repr);
                })*

        }
    }

        precedence_tests! {
            minusatimesb: ("-a * b", "((-a) * b)"),
            notminusa: ("!-a", "(!(-a))"),
            aplusbplusc: ("a+b+c", "((a + b) + c)"),
            aplusbminusc: ("a + b - c", "((a + b) - c)"),
            atimesbtimesc: ("a * b * c", "((a * b) * c)"),
            atimebdivc: ("a * b / c", "((a * b) / c)"),
            aplusbdivc: ("a + b / c", "(a + (b / c))"),
            lotsavars: ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            twointstatements: ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            gteqlt: ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ltneqgt: ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            mathseq: ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        }
    }
}
