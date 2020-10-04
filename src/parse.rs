use std::mem;

use crate::lex;

use crate::lex::TokenType::*;

type Arena<'a> = Vec<Box<dyn ASTNode + 'a>>;

pub mod ast {
    use super::{lex, Arena};
    use std::fmt;
    use std::ops::Deref;

    pub trait ASTNode: fmt::Debug + fmt::Display {
        fn get_token_literal(&self) -> Option<&str>;
        fn get_expression_kind(&self) -> Option<&ExpressionKind>;
        fn get_statement_kind(&self) -> Option<&StatementKind>;
    }

    #[derive(Debug)]
    pub struct NodeRef<'a> {
        index: usize,
        arena: &'a Arena<'a>,
    }

    impl<'a> Deref for NodeRef<'a> {
        type Target = Box<dyn ASTNode + 'a>;

        fn deref<'b>(&'b self) -> &'b Self::Target {
            self.arena.get(self.index).unwrap()
        }
    }

    impl<'a> fmt::Display for NodeRef<'a> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            // Delegate to referenced ASTNode
            self.deref().fmt(f)
        }
    }

    #[derive(Debug)]
    pub enum ExpressionKind<'a> {
        BooleanLiteral(bool),
        Identifier(lex::Token<'a>),
        InfixExpression {
            left: NodeRef<'a>,
            operator: lex::Token<'a>,
            right: NodeRef<'a>,
        },
        IntegerLiteral(i64),
        PrefixExpression {
            operator: lex::Token<'a>,
            right: NodeRef<'a>,
        },
        IfConditional {
            condition: NodeRef<'a>,
            consequence: NodeRef<'a>,
            alternative: Option<NodeRef<'a>>,
        },
        FunctionLiteral {
            parameters: Vec<lex::Token<'a>>,
            body: NodeRef<'a>,
        },
        CallExpression {
            function: &'a str,
            arguments: Vec<NodeRef<'a>>,
        },
    }

    #[derive(Debug)]
    pub struct ExpressionNode<'a> {
        pub token: lex::Token<'a>,
        pub kind: ExpressionKind<'a>,
    }

    impl ASTNode for ExpressionNode<'_> {
        fn get_token_literal(&self) -> Option<&str> {
            self.token.get_literal()
        }
        fn get_statement_kind(&self) -> Option<&StatementKind> {
            None
        }
        fn get_expression_kind(&self) -> Option<&ExpressionKind> {
            Some(&self.kind)
        }
    }

    impl<'a> fmt::Display for ExpressionNode<'a> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match &self.kind {
                ExpressionKind::BooleanLiteral(value) => write!(f, "{}", value),
                ExpressionKind::Identifier(name) => write!(f, "{}", name.get_literal().unwrap()),
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
                ExpressionKind::IntegerLiteral(value) => write!(f, "{}", value),
                ExpressionKind::PrefixExpression { operator, right } => {
                    write!(f, "({}{})", operator.get_literal().unwrap(), right)
                }
                ExpressionKind::IfConditional {
                    condition,
                    consequence,
                    alternative,
                } => match &alternative {
                    Some(a) => write!(f, "if {} {} else {}", condition, consequence, a),
                    None => write!(f, "if {} {}", condition, consequence),
                },
                ExpressionKind::FunctionLiteral { parameters, body } => write!(
                    f,
                    "fn ({}) {}",
                    parameters
                        .into_iter()
                        .map(|s| format!("{}", s))
                        .collect::<Vec<String>>()
                        .join(", "),
                    body
                ),
                ExpressionKind::CallExpression {
                    function,
                    arguments,
                } => write!(
                    f,
                    "{}({})",
                    function,
                    arguments
                        .into_iter()
                        .map(|s| format!("{}", s))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
            }
        }
    }

    #[derive(Debug)]
    pub enum StatementKind<'a> {
        Let {
            name: lex::Token<'a>,
            value: NodeRef<'a>,
        },
        Return(NodeRef<'a>),
        Expression(NodeRef<'a>),
        Block(Vec<NodeRef<'a>>),
    }

    #[derive(Debug)]
    pub struct StatementNode<'a> {
        pub token: lex::Token<'a>,
        pub kind: StatementKind<'a>,
    }

    impl<'a> ASTNode for StatementNode<'a> {
        fn get_token_literal(&self) -> Option<&str> {
            self.token.get_literal()
        }
        fn get_statement_kind(&self) -> Option<&StatementKind> {
            Some(&self.kind)
        }
        fn get_expression_kind(&self) -> Option<&ExpressionKind> {
            None
        }
    }

    impl<'a> fmt::Display for StatementNode<'a> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match &self.kind {
                StatementKind::Let { name, value } => write!(
                    f,
                    "{} {} = {};",
                    self.get_token_literal().unwrap(),
                    name.get_literal().unwrap(),
                    value
                ),
                StatementKind::Return(value) => {
                    write!(f, "{} {};", self.get_token_literal().unwrap(), value)
                }
                StatementKind::Expression(expression) => write!(f, "{}", expression),
                StatementKind::Block(statements) => {
                    for statement in statements {
                        write!(f, "{}", statement)?;
                    }
                    write!(f, "")
                }
            }
        }
    }

    /// Root node of all Monkey ASTs
    pub struct Program<'a> {
        pub statements: Vec<NodeRef<'a>>,
        arena: Arena<'a>,
    }

    impl<'a> Program<'a> {
        pub fn new(statements: Vec<NodeRef<'a>>, arena: Vec<Box<dyn ASTNode + 'a>>) -> Self {
            Self { statements, arena }
        }
    }

    impl<'a> fmt::Display for Program<'a> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            for statement in &self.statements {
                writeln!(f, "{}", statement)?;
            }
            f.write_str("")
        }
    }

    // #[cfg(test)]
    // mod test {
    // use super::*;
    // #[test]
    // fn test_display_impl() {
    // let input = "let myvar = anotherVar;";
    // let lexer = lex::Lexer::for_str(&input);
    // let parser = super::Parser::for_lexer(lexer);
    // let prog = Program {
    // statements: vec![StatementNode {
    // token: lex::Token::with_str(lex::TokenType::LET, "let"),
    // kind: StatementKind::Let {
    // name: lex::Token::with_str(lex::TokenType::IDENT, "myVar"),
    // value: ExpressionNode {
    // token: lex::Token::with_str(lex::TokenType::IDENT, "anotherVar"),
    // kind: ExpressionKind::Identifier(lex::Token::with_str(
    // lex::TokenType::IDENT,
    // "anotherVar",
    // )),
    // },
    // },
    // }],
    // };
    //
    // let prog_string = format!("{}", prog);
    // assert_eq!(prog_string, "let myVar = anotherVar;\n");
    // }
    // }
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
        lex::TokenType::LPAREN => OperatorPrecedence::CALL,
        _ => OperatorPrecedence::LOWEST,
    }
}

pub struct Parser<'a> {
    lexer: lex::Lexer<'a>,
    current_token: lex::Token<'a>,
    peek_token: Option<lex::Token<'a>>,
    errors: Vec<String>,
    arena: Arena<'a>,
}

impl<'a> Parser<'a> {
    pub fn for_lexer(mut lexer: lex::Lexer) -> Parser {
        let first_token = lexer.next_token().expect("Passed an empty lexer");
        let second_token = lexer.next_token().expect("Passed an empty lexer");

        Parser {
            lexer,
            current_token: first_token,
            peek_token: Some(second_token),
            errors: Vec::new(),
            arena: Vec::new(),
        }
    }

    pub fn add_node(&'a mut self, node: Box<dyn ASTNode + 'a>) -> NodeRef<'a> {
        self.arena.push(node);
        NodeRef {
            index: self.arena.len() - 1,
            arena: &self.arena,
        }
    }

    pub fn advance_tokens(&'a mut self) -> lex::Token<'a> {
        let next = self.lexer.next_token().expect("Unable to advance token");
        let mut peek_unwrapped: lex::Token = self.peek_token.take().unwrap();
        mem::swap(&mut peek_unwrapped, &mut self.current_token);
        self.peek_token = Some(next);
        peek_unwrapped
    }

    pub fn parse(&'a mut self) -> Result<Program<'a>, String> {
        let mut statements = Vec::new();
        while !self.is_current_token_of_type(EOF) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => self.errors.push(String::from(err)),
            }
            self.advance_tokens();
        }
        if self.errors.is_empty() {
            Ok(Program::new(statements, mem::take(&mut self.arena)))
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

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn parse_statement(&'a mut self) -> Result<NodeRef<'a>, String> {
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

    fn advance_tokens_if_next_of_type(
        &'a mut self,
        expected_type: lex::TokenType,
    ) -> Result<lex::Token<'a>, String> {
        if let Some(t) = &self.peek_token {
            if &t.token_type == &expected_type {
                Ok(self.advance_tokens())
            } else {
                Err(format!(
                    "Expecting {} but next token is {}",
                    expected_type, &t
                ))
            }
        } else {
            Err(format!(
                "Expecting {} but no next token found",
                expected_type
            ))
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

    fn parse_let_statement(&'a mut self) -> Result<NodeRef<'a>, String> {
        let original_token = self.advance_tokens_if_next_of_type(IDENT)?;
        let ident_token = self.advance_tokens_if_next_of_type(ASSIGN)?;
        self.advance_tokens();
        let value = self.parse_expression(OperatorPrecedence::LOWEST)?;
        if self.is_next_token_of_type(SEMICOLON) {
            self.advance_tokens();
        }
        Ok(self.add_node(Box::new(StatementNode {
            token: original_token,
            kind: StatementKind::Let {
                name: ident_token,
                value,
            },
        })))
    }

    fn parse_return_statement(&'a mut self) -> Result<NodeRef<'a>, String> {
        let original_token = self.advance_tokens();
        let value = self.parse_expression(OperatorPrecedence::LOWEST)?;
        if self.is_next_token_of_type(SEMICOLON) {
            self.advance_tokens();
        }
        Ok(self.add_node(Box::new(StatementNode {
            token: original_token,
            kind: StatementKind::Return(value),
        })))
    }

    fn attempt_parse_expression_statement(&'a mut self) -> Result<NodeRef<'a>, String> {
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

        Ok(self.add_node(Box::new(StatementNode {
            token: original_token,
            kind: StatementKind::Expression(result),
        })))
    }

    fn parse_expression(
        &'a mut self,
        precedence: OperatorPrecedence,
    ) -> Result<NodeRef<'a>, String> {
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

    fn attempt_prefix_parse(&'a mut self) -> Result<Option<NodeRef<'a>>, String> {
        let parse_attempt = match &self.current_token.token_type {
            lex::TokenType::IDENT => self.parse_identifier()?,
            lex::TokenType::INT => self.parse_integer_literal()?,
            lex::TokenType::BANG => self.parse_prefix_expression()?,
            lex::TokenType::MINUS => self.parse_prefix_expression()?,
            lex::TokenType::TRUE => self.parse_boolean_literal()?,
            lex::TokenType::FALSE => self.parse_boolean_literal()?,
            lex::TokenType::LPAREN => self.parse_grouped_expression()?,
            lex::TokenType::IF => self.parse_if_expression()?,
            lex::TokenType::FUNCTION => self.parse_function_literal()?,
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
                lex::TokenType::LPAREN => true,
                _ => false,
            },
        }
    }

    fn parse_identifier(&'a mut self) -> Result<NodeRef<'a>, String> {
        Ok(self.add_node(Box::new(ExpressionNode {
            token: self.current_token.clone(),
            kind: ExpressionKind::Identifier(self.current_token),
        })))
    }

    fn parse_integer_literal(&'a mut self) -> Result<NodeRef<'a>, String> {
        let token = self.current_token.clone();
        let lit = token.get_literal().unwrap();
        let attempted_parse = lit.parse();
        match attempted_parse {
            Ok(value) => Ok(self.add_node(Box::new(ExpressionNode {
                token,
                kind: ExpressionKind::IntegerLiteral(value),
            }))),
            Err(_err) => Err(format!("Cannot parse '{}' to i64", lit)),
        }
    }

    fn parse_boolean_literal(&'a mut self) -> Result<NodeRef<'a>, String> {
        let token = self.current_token.clone();
        let value = match &token.token_type {
            lex::TokenType::TRUE => true,
            lex::TokenType::FALSE => false,
            _ => {
                return Err(format!(
                    "Expected TRUE/FALSE, got token type {}",
                    &token.token_type
                ))
            }
        };

        Ok(self.add_node(Box::new(ExpressionNode {
            token,
            kind: ExpressionKind::BooleanLiteral(value),
        })))
    }

    fn parse_grouped_expression(&'a mut self) -> Result<NodeRef<'a>, String> {
        self.advance_tokens();
        let expr = self.parse_expression(OperatorPrecedence::LOWEST);
        if !self.is_next_token_of_type(lex::TokenType::RPAREN) {
            let s = match &self.peek_token {
                Some(t) => t.get_literal().unwrap().as_ref(),
                None => "EOF",
            };

            Err(format!("Group parse expecting RPAREN, got {}", s))
        } else {
            self.advance_tokens();
            expr
        }
    }

    fn parse_if_expression(&'a mut self) -> Result<NodeRef<'a>, String> {
        if !self.is_next_token_of_type(lex::TokenType::LPAREN) {
            return Err(format!("Expecting LPAREN, got {:?}", self.peek_token));
        }
        let original_token = self.advance_tokens();
        self.advance_tokens();
        let condition = self.parse_expression(OperatorPrecedence::LOWEST)?;
        if !self.is_next_token_of_type(lex::TokenType::RPAREN) {
            return Err(format!("Expecting RPAREN, got {:?}", self.peek_token));
        }
        self.advance_tokens();
        if !self.is_next_token_of_type(lex::TokenType::LBRACE) {
            return Err(format!("Expecting LBRACE, got {:?}", self.peek_token));
        }
        self.advance_tokens();

        let consequence = self.parse_block_statement()?;

        if self.is_next_token_of_type(lex::TokenType::ELSE) {
            self.advance_tokens();
            if !self.is_next_token_of_type(lex::TokenType::LBRACE) {
                return Err(format!("Expecting LBRACE, got {:?}", self.peek_token));
            }
            self.advance_tokens();

            let alternative = self.parse_block_statement()?;
            Ok(self.add_node(Box::new(ExpressionNode {
                token: original_token,
                kind: ExpressionKind::IfConditional {
                    condition,
                    consequence,
                    alternative: Some(alternative),
                },
            })))
        } else {
            Ok(self.add_node(Box::new(ExpressionNode {
                token: original_token,
                kind: ExpressionKind::IfConditional {
                    condition,
                    consequence,
                    alternative: None,
                },
            })))
        }
    }

    fn parse_prefix_expression(&'a mut self) -> Result<NodeRef<'a>, String> {
        let operator_token = self.advance_tokens();

        let rhs = self.parse_expression(OperatorPrecedence::PREFIX)?;
        Ok(self.add_node(Box::new(ExpressionNode {
            token: operator_token.clone(),
            kind: ExpressionKind::PrefixExpression {
                operator: operator_token,
                right: rhs,
            },
        })))
    }

    fn parse_infix_expression(&'a mut self, lhs: NodeRef<'a>) -> Result<NodeRef<'a>, String> {
        let current_precedence = self.get_current_token_precedence();
        let expr_token = self.advance_tokens();
        match &expr_token.token_type {
            lex::TokenType::LPAREN => Ok(self.parse_call_expression(expr_token, lhs)?),
            _ => {
                let rhs = self.parse_expression(current_precedence)?;
                Ok(self.add_node(Box::new(ExpressionNode {
                    token: expr_token.clone(),
                    kind: ExpressionKind::InfixExpression {
                        left: lhs,
                        operator: expr_token,
                        right: rhs,
                    },
                })))
            }
        }
    }

    fn parse_block_statement(&'a mut self) -> Result<NodeRef<'a>, String> {
        let mut statements = Vec::new();
        let token = self.advance_tokens();
        while !self.is_current_token_of_type(lex::TokenType::RBRACE)
            && !self.is_current_token_of_type(lex::TokenType::EOF)
        {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.advance_tokens();
        }
        Ok(self.add_node(Box::new(StatementNode {
            token,
            kind: StatementKind::Block(statements),
        })))
    }

    fn parse_function_literal(&'a mut self) -> Result<NodeRef<'a>, String> {
        let original_token = self.advance_tokens_if_next_of_type(lex::TokenType::LPAREN)?;
        let parameters = self.parse_function_parameters()?;

        self.advance_tokens_if_next_of_type(lex::TokenType::LBRACE)?;
        let body = self.parse_block_statement()?;
        Ok(self.add_node(Box::new(ExpressionNode {
            token: original_token,
            kind: ExpressionKind::FunctionLiteral { parameters, body },
        })))
    }

    fn parse_function_parameters(&'a mut self) -> Result<Vec<lex::Token<'a>>, String> {
        let mut params = Vec::new();
        if self.is_next_token_of_type(lex::TokenType::RPAREN) {
            self.advance_tokens();
        } else {
            self.advance_tokens();
            while self.is_next_token_of_type(lex::TokenType::COMMA) {
                params.push(self.advance_tokens());
                self.advance_tokens();
            }
            params.push(self.advance_tokens_if_next_of_type(lex::TokenType::RPAREN)?);
        }
        Ok(params)
    }

    fn parse_call_expression(
        &'a mut self,
        original_token: lex::Token<'a>,
        function: NodeRef<'a>,
    ) -> Result<NodeRef<'a>, String> {
        let arguments = self.parse_call_arguments()?;
        let fn_name = function.get_token_literal().unwrap();
        Ok(self.add_node(Box::new(ExpressionNode {
            token: original_token.clone(),
            kind: ExpressionKind::CallExpression {
                function: fn_name,
                arguments,
            },
        })))
    }

    fn parse_call_arguments(&'a mut self) -> Result<Vec<NodeRef<'a>>, String> {
        let mut params = Vec::new();
        if self.is_next_token_of_type(lex::TokenType::RPAREN) {
            self.advance_tokens();
        } else {
            params.push(self.parse_expression(OperatorPrecedence::LOWEST)?);
            while self.is_next_token_of_type(lex::TokenType::COMMA) {
                self.advance_tokens();
                self.advance_tokens();
                params.push(self.parse_expression(OperatorPrecedence::LOWEST)?);
            }
            self.advance_tokens_if_next_of_type(lex::TokenType::RPAREN)?;
        }
        Ok(params)
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

    fn check_is_identifier(name: &NodeRef, expected_name: &str) {
        if let Some(kind) = name.get_expression_kind() {
            match &kind {
                ExpressionKind::Identifier(name) => {
                    assert_eq!(
                        name, name,
                        "Expecting ident '{}', got '{}'",
                        expected_name, name
                    );
                }
                _ => assert!(false, "Expression is not an Identifier"),
            }
        } else {
            assert!(false, "Node is not an Expression")
        }
    }

    fn check_is_int_literal(expression: &NodeRef, expected_value: i64) {
        if let Some(kind) = expression.get_expression_kind() {
            match kind {
                ExpressionKind::IntegerLiteral(value) => {
                    assert_eq!(
                        *value, expected_value,
                        "Expecting value '{}', got '{}'",
                        expected_value, value
                    );
                }
                _ => assert!(false, "Expression is not an IntegerLiteral"),
            }
        } else {
            assert!(false, "Node is not an Expression")
        }
    }

    fn check_is_boolean(expression: &NodeRef, expected_value: bool) {
        if let Some(kind) = expression.get_expression_kind() {
            match kind {
                ExpressionKind::BooleanLiteral(value) => {
                    assert_eq!(
                        *value, expected_value,
                        "Expecting {}, got {}",
                        expected_value, value
                    );
                }
                _ => assert!(false, "Expression is not a BooleanLiteral"),
            }
        } else {
            assert!(false, "Node is not an Expression")
        }
    }

    fn check_infix_expression(expression: &NodeRef, expected_str: &str) {
        if let Some(kind) = expression.get_expression_kind() {
            if let ExpressionKind::InfixExpression {
                left: _,
                operator: _,
                right: _,
            } = kind
            {
                assert_eq!(format!("{}", expression), expected_str);
            } else {
                assert!(false, "Expression is not an InfixExpression");
            }
        } else {
            assert!(false, "Node not an expression")
        }
    }

    mod test_let_statements {
        use super::*;
        macro_rules! let_tests {
        ($($name:ident: $value:expr, $checkfn:ident,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_ident, expected_value) = $value;
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
                    let statement = *program.statements.get(0).unwrap();
                    if let Some(kind) = statement.get_statement_kind() {
                    match kind {
                        StatementKind::Let { name, value } => {
                            assert_eq!(name.get_literal().unwrap(), expected_ident);
                            $checkfn(value, expected_value);
                        }
                        _ => assert!(false, "Encountered unexpected non-let statement"),
                    }} else {
                        assert!(false, "Encountered unexpected expression");
                    }

                })*
        }
    }

        let_tests! {
            xfive: ("let x = 5;", "x", 5), check_is_int_literal,
            ytrue: ("let y = true;", "y", true), check_is_boolean,
            foobary: ("let foobar = y;", "foobar", "y"), check_is_identifier,
        }
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
        let expected_idents = vec![("x", 5), ("y", 10), ("foobar", 838383)];
        let pairs = program.statements.iter().zip(expected_idents);
        for (statement, ident) in pairs {
            match &statement.get_statement_kind().unwrap() {
                StatementKind::Let { name, value } => {
                    assert_eq!(name.get_literal().unwrap(), ident.0);
                    check_is_int_literal(value, ident.1);
                }
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
            "Expecting ASSIGN but next token is INT(\"5\")",
            "Expecting IDENT but next token is ASSIGN(\"=\")",
            "Cannot parse expression statement from ASSIGN(\"=\")",
            "Expecting IDENT but next token is INT(\"838383\")",
        ];
        for (actual, expected) in parser.errors.iter().zip(expected_errors) {
            assert_eq!(
                actual, expected,
                "Error did not match expectation: wanted {}, got {}",
                expected, actual
            );
        }
    }

    mod test_return_statements {
        use super::*;
        macro_rules! return_tests {
        ($($name:ident: $value:expr, $checkfn:ident,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_value) = $value;
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
                    match &statement.get_statement_kind().unwrap() {
                        StatementKind::Return ( value ) => {
                            $checkfn(value, expected_value);
                        }
                        _ => assert!(false, "Encountered unexpected non-return statement"),
                    }

                })*
        }
    }

        return_tests! {
            retfive: ("return 5;", 5), check_is_int_literal,
            rettrue: ("return true;", true), check_is_boolean,
            retident: ("return y;", "y"), check_is_identifier,
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
        let expected_ints = vec![5, 10, 838383];
        for (statement, expected) in program.statements.iter().zip(expected_ints) {
            match &statement.get_statement_kind().unwrap() {
                StatementKind::Return(value) => {
                    assert_eq!(
                        statement.get_token_literal().unwrap(),
                        "return",
                        "Return statement has non-return token"
                    );
                    check_is_int_literal(value, expected);
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
        match &statement.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                check_is_identifier(expression, "foobar");
            }
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
        match &statement.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                check_is_int_literal(expression, 5);
            }
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    mod test_prefix_funcs {
        use super::*;
        macro_rules! prefix_tests {
        ($($name:ident: $value:expr, $checkfn:ident,)*) => {
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

                    match &statement.get_statement_kind().unwrap() {
                        StatementKind::Expression ( expression ) => match &expression.get_expression_kind().unwrap() {
                            ExpressionKind::PrefixExpression { operator, right } => {
                                assert_eq!(
                                    operator.get_literal().unwrap(),
                                    expected_operator
                                );
                                $checkfn(&*right, expected_value);
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
            notfive: ("!5", "!", 5), check_is_int_literal,
            negfifteen: ("-15", "-", 15), check_is_int_literal,
            nottrue: ("!true", "!", true), check_is_boolean,
            notfalse: ("!false", "!", false), check_is_boolean,
        }
    }

    mod test_infix_funcs {
        use super::*;
        macro_rules! infix_tests {
        ($($name:ident: $value:expr, $checkfn:ident,)*) => {
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

                    match &statement.get_statement_kind().unwrap() {
                        StatementKind::Expression ( expression ) => match &expression.get_expression_kind().unwrap() {
                            ExpressionKind::InfixExpression { left, operator, right } => {
                                assert_eq!(
                                    operator.get_literal().unwrap(),
                                    expected_operator
                                );
                                $checkfn(left, expected_lhs);
                                $checkfn(right, expected_rhs);
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
            plus: ("5 + 5;", 5, "+", 5), check_is_int_literal,
            minus: ("5 - 5;", 5, "-", 5), check_is_int_literal,
            asterisk: ("5 * 5", 5, "*", 5), check_is_int_literal,
            slash: ("5 / 5", 5, "/", 5), check_is_int_literal,
            gt: ("5 > 5", 5, ">", 5), check_is_int_literal,
            lt: ("5 < 5", 5, "<", 5), check_is_int_literal,
            eqeq: ("5 == 5", 5, "==", 5), check_is_int_literal,
            noteq: ("5 != 5", 5, "!=", 5), check_is_int_literal,
            trueeqtrue: ("true == true", true, "==", true), check_is_boolean,
            trueneqfalse: ("true != false", true, "!=", false), check_is_boolean,
            falseeqfalse: ("false == false", false, "==", false), check_is_boolean,
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
            falsecompare: ("3 > 5 == false", "((3 > 5) == false)"),
            truecompare: ("3 < 5 == true", "((3 < 5) == true)"),
            parenssimple: ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            parenplusmult: ("(5 + 5) * 2", "((5 + 5) * 2)"),
            parendivplus: ("2 / (5 + 5)", "(2 / (5 + 5))"),
            parenunary: ("-(5 + 5)", "(-(5 + 5))"),
            parenbool: ("!(true == true)", "(!(true == true))"),
            simplecall: ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            nestcall: ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            callbigexpr: ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
        }
    }

    #[test]
    fn test_parse_boolean_expression() {
        let input = "true; false;";
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
            length, 2,
            "Expecting program to contain 2 statements, got {}",
            length
        );
        let statement_true = program.statements.get(0).unwrap();
        match &statement_true.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                check_is_boolean(expression, true);
            }
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
        let statement_false = program.statements.get(1).unwrap();
        match &statement_false.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                check_is_boolean(expression, false);
            }
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    #[test]
    fn test_parse_if_expression() {
        let input = "if (x < y) { x }";
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
        match &statement.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                match &expression.get_expression_kind().unwrap() {
                    ExpressionKind::IfConditional {
                        condition,
                        consequence,
                        alternative,
                    } => {
                        assert_eq!(format!("{}", condition), "(x < y)");
                        if let StatementKind::Block(statements) =
                            *consequence.get_statement_kind().unwrap()
                        {
                            assert_eq!(statements.len(), 1);
                            assert_eq!(format!("{}", consequence), "x");
                        } else {
                            assert!(false, "Consequence not a block statement");
                        }
                        assert!(alternative.is_none())
                    }
                    _ => assert!(
                        false,
                        "Expecting IfConditional, got {:?}",
                        expression.get_expression_kind()
                    ),
                }
            }
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    #[test]
    fn test_parse_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
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
        match statement.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                match expression.get_expression_kind().unwrap() {
                    ExpressionKind::IfConditional {
                        condition,
                        consequence,
                        alternative,
                    } => {
                        assert_eq!(format!("{}", condition), "(x < y)");
                        if let StatementKind::Block(statements) =
                            *consequence.get_statement_kind().unwrap()
                        {
                            assert_eq!(statements.len(), 1);
                            assert_eq!(format!("{}", statements.get(0).unwrap()), "x");
                            assert_eq!(format!("{}", consequence), "x");
                        } else {
                            assert!(false, "Consequence not a block statement");
                        }
                        if let StatementKind::Block(statements) =
                            *alternative.unwrap().get_statement_kind().unwrap()
                        {
                            assert_eq!(statements.len(), 1);
                            assert_eq!(format!("{}", statements.get(0).unwrap()), "y")
                        } else {
                            assert!(false, "Alternative not a block statement");
                        }
                    }
                    _ => assert!(
                        false,
                        "Expecting IfConditional, got {:?}",
                        expression.get_expression_kind().unwrap()
                    ),
                }
            }
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    #[test]
    fn test_parse_function_expression() {
        let input = "fn(x, y) { x + y }";
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
        match statement.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                match expression.get_expression_kind().unwrap() {
                    ExpressionKind::FunctionLiteral { parameters, body } => {
                        assert_eq!(
                            parameters.len(),
                            2,
                            "Expecting two params, got {:?}",
                            parameters
                        );
                        assert_eq!(parameters.get(0).unwrap().get_literal().unwrap(), "x");
                        assert_eq!(parameters.get(1).unwrap().get_literal().unwrap(), "y");
                        if let StatementKind::Block(statements) =
                            *body.get_statement_kind().unwrap()
                        {
                            assert_eq!(statements.len(), 1);
                            assert_eq!(format!("{}", statements.get(0).unwrap()), "(x + y)")
                        } else {
                            assert!(false, "Body not a block statement");
                        }
                    }
                    _ => assert!(
                        false,
                        "Expecting FunctionLiteral, got {:?}",
                        expression.get_expression_kind().unwrap()
                    ),
                }
            }
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    #[test]
    fn test_parse_function_expression_one_param() {
        let input = "fn(x) { x }";
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
        match statement.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                match expression.get_expression_kind().unwrap() {
                    ExpressionKind::FunctionLiteral { parameters, body } => {
                        assert_eq!(
                            parameters.len(),
                            1,
                            "Expecting one param, got {:?}",
                            parameters
                        );
                        assert_eq!(parameters.get(0).unwrap().get_literal().unwrap(), "x");
                        if let StatementKind::Block(statements) =
                            *body.get_statement_kind().unwrap()
                        {
                            assert_eq!(statements.len(), 1);
                            assert_eq!(format!("{}", statements.get(0).unwrap()), "x")
                        } else {
                            assert!(false, "Body not a block statement");
                        }
                    }
                    _ => assert!(
                        false,
                        "Expecting FunctionLiteral, got {:?}",
                        expression.get_expression_kind().unwrap()
                    ),
                }
            }
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    #[test]
    fn test_parse_function_expression_no_params() {
        let input = "fn() { x + y }";
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
        match &statement.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => match &expression
                .get_expression_kind()
                .unwrap()
            {
                ExpressionKind::FunctionLiteral { parameters, body } => {
                    assert_eq!(
                        parameters.len(),
                        0,
                        "Expecting zero params, got {:?}",
                        parameters
                    );
                    if let StatementKind::Block(statements) = *body.get_statement_kind().unwrap() {
                        assert_eq!(statements.len(), 1);
                        assert_eq!(format!("{}", statements.get(0).unwrap()), "(x + y)")
                    } else {
                        assert!(false, "Body not a block statement");
                    }
                }
                _ => assert!(
                    false,
                    "Expecting FunctionLiteral, got {:?}",
                    expression.get_expression_kind().unwrap()
                ),
            },
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }

    #[test]
    fn test_parse_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = lex::Lexer::for_str(input);
        let mut parser = Parser::for_lexer(lexer);
        let parsed = parser.parse();
        assert_ne!(
            parsed.is_err(),
            true,
            "Parser failed with errors \"{:?}\"",
            &parser.errors
        );
        let program = parsed.unwrap();
        let length = program.statements.len();
        assert_eq!(
            length, 1,
            "Expecting program to contain 1 statement, got {}",
            length
        );
        let statement = program.statements.get(0).unwrap();
        match statement.get_statement_kind().unwrap() {
            StatementKind::Expression(expression) => {
                match expression.get_expression_kind().unwrap() {
                    ExpressionKind::CallExpression {
                        function,
                        arguments,
                    } => {
                        assert_eq!(function, &"add");
                        assert_eq!(arguments.len(), 3);
                        check_is_int_literal(arguments.get(0).unwrap(), 1);
                        check_infix_expression(arguments.get(1).unwrap(), "(2 * 3)");
                        check_infix_expression(arguments.get(2).unwrap(), "(4 + 5)");
                    }
                    _ => assert!(
                        false,
                        "Expecting CallExpression, got {:?}",
                        expression.get_expression_kind().unwrap()
                    ),
                }
            }
            _ => assert!(false, "Encountered unexpected non-expression statement"),
        }
    }
}
