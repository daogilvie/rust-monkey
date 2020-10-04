use crate::lex::{Token, TokenType};
use crate::object::*;
use crate::parse;
use crate::parse::ast::{ExpressionKind, NodeRef, StatementKind};
use std::cell::UnsafeCell;
use std::collections::HashMap;

struct EvalResult {
    pub result: Object,
    pub is_return: bool,
}

impl EvalResult {
    fn with_object_and_return(result: Object, is_return: bool) -> Self {
        Self { result, is_return }
    }

    fn with_object(result: Object) -> Self {
        Self {
            result,
            is_return: false,
        }
    }
}

pub struct Environment {
    identifiers: UnsafeCell<HashMap<String, Object>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            identifiers: UnsafeCell::new(HashMap::new()),
        }
    }

    fn upsert_ident(&self, ident: &str, value: Object) {
        // SAFETY: I know that no interior member of the hashmap has given
        // out an external reference, and that only one update can be happening
        // at a time as UnsafeCell ==> !Sync
        unsafe {
            let map = &mut *self.identifiers.get();
            map.insert(String::from(ident), value);
        }
    }

    fn get_current_value(&self, ident: &str) -> Option<Object> {
        // SAFETY: We clone on output here so that the unsafe insert can be done
        // above. We know that no other thread is accessing this.
        let o = unsafe { (&*self.identifiers.get()).get(ident) };
        if o.is_some() {
            Some(o.unwrap().clone())
        } else {
            None
        }
    }
}

/// Monkey evaluates positive numbers and true as truthy
fn is_truthy(obj: &Object) -> bool {
    match obj.get_type() {
        ObjectType::Boolean(b) => return b,
        ObjectType::Null => false,
        ObjectType::Integer(i) => i > 0,
    }
}

fn eval_prefix_expression(operator: &Token, right: Object) -> Result<Object, String> {
    match &operator.token_type {
        TokenType::BANG => {
            if is_truthy(&right) {
                Ok(FALSE)
            } else {
                Ok(TRUE)
            }
        }
        TokenType::MINUS => match right.get_type() {
            ObjectType::Integer(i) => Ok(Object::with_type(ObjectType::Integer(-i))),
            _ => Err(format!("undefined prefix operation: -{}", right.get_type())),
        },
        _ => Err(format!("Cannot eval prefix from {}", operator)),
    }
}

fn eval_identifier(ident: Token, environment: &Environment) -> Result<Object, String> {
    let ident_name = ident.get_literal().unwrap();
    match environment.get_current_value(ident_name) {
        Some(o) => Ok(o),
        None => Err(format!(
            "undefined identifier: {}",
            ident.get_literal().unwrap()
        )),
    }
}

fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Result<Object, String> {
    match (left.get_type(), right.get_type()) {
        (ObjectType::Integer(int_l), ObjectType::Integer(int_r)) => match &operator.token_type {
            TokenType::PLUS => Ok(Object::with_type(ObjectType::Integer(int_l + int_r))),
            TokenType::MINUS => Ok(Object::with_type(ObjectType::Integer(int_l - int_r))),
            TokenType::ASTERISK => Ok(Object::with_type(ObjectType::Integer(int_l * int_r))),
            TokenType::SLASH => Ok(Object::with_type(ObjectType::Integer(int_l / int_r))),
            TokenType::EQ => Ok(Object::with_type(ObjectType::Boolean(int_l == int_r))),
            TokenType::NOTEQ => Ok(Object::with_type(ObjectType::Boolean(int_l != int_r))),
            TokenType::LT => Ok(Object::with_type(ObjectType::Boolean(int_l < int_r))),
            TokenType::GT => Ok(Object::with_type(ObjectType::Boolean(int_l > int_r))),
            _ => Err(format!(
                "Cannot eval operator {} with integer operands",
                operator
            )),
        },
        (ObjectType::Boolean(bool_l), ObjectType::Boolean(bool_r)) => match &operator.token_type {
            TokenType::EQ => Ok(Object::with_type(ObjectType::Boolean(bool_l == bool_r))),
            TokenType::NOTEQ => Ok(Object::with_type(ObjectType::Boolean(bool_l != bool_r))),
            _ => Err(format!(
                "undefined operation: BOOLEAN {} BOOLEAN",
                operator.get_literal().unwrap()
            )),
        },
        _ => Err(format!(
            "type mismatch: {} {} {}",
            left.get_type(),
            operator.get_literal().unwrap(),
            right.get_type()
        )),
    }
}

fn eval_expression(expr: NodeRef, environment: &Environment) -> Result<EvalResult, String> {
    if let Some(kind) = expr.get_expression_kind() {
        match kind {
            ExpressionKind::IntegerLiteral(value) => Ok(EvalResult::with_object(
                Object::with_type(ObjectType::Integer(*value)),
            )),
            ExpressionKind::BooleanLiteral(value) => {
                if *value {
                    Ok(EvalResult::with_object(TRUE))
                } else {
                    Ok(EvalResult::with_object(FALSE))
                }
            }
            ExpressionKind::PrefixExpression { operator, right } => {
                let eval_r = eval_expression(*right, environment)?;
                Ok(EvalResult::with_object(eval_prefix_expression(
                    &operator,
                    eval_r.result,
                )?))
            }
            ExpressionKind::InfixExpression {
                left,
                operator,
                right,
            } => {
                let eval_l = eval_expression(*left, environment)?;
                let eval_r = eval_expression(*right, environment)?;
                Ok(EvalResult::with_object(eval_infix_expression(
                    *operator,
                    eval_l.result,
                    eval_r.result,
                )?))
            }
            ExpressionKind::IfConditional {
                condition,
                consequence,
                alternative,
            } => {
                let eval_c = eval_expression(*condition, environment)?;
                if is_truthy(&eval_c.result) {
                    eval_statement(*consequence, environment)
                } else if let Some(alt) = alternative {
                    eval_statement(*alt, environment)
                } else {
                    Ok(EvalResult::with_object(Object::with_type(ObjectType::Null)))
                }
            }
            ExpressionKind::Identifier(name) => {
                Ok(EvalResult::with_object(eval_identifier(*name, environment)?))
            }
            _ => Err(format!(
                "Cannot eval '{:?}'",
                expr.get_expression_kind().unwrap()
            )),
        }
    } else {
        Err(String::from("Attempted to eval statement as expression"))
    }
}

fn eval_statement(statement: NodeRef, environment: &Environment) -> Result<EvalResult, String> {
    if let Some(kind) = statement.get_statement_kind() {
        match kind {
            StatementKind::Expression(expression) => eval_expression(*expression, environment),
            StatementKind::Return(value) => {
                let mut r = eval_expression(*value, environment)?;
                r.is_return = true;
                Ok(r)
            }
            StatementKind::Let { name, value } => {
                let v = eval_expression(*value, environment)?;
                environment.upsert_ident(name.get_literal().unwrap(), v.result.clone());
                Ok(EvalResult::with_object(v.result))
            }
            StatementKind::Block(statements) => {
                let mut result;
                for statement in statements {
                    let inner_result = eval_statement(*statement, environment)?;
                    let returning = inner_result.is_return;
                    result = Ok(inner_result);
                    if returning {
                        break;
                    }
                }
                result
            }
        }
    } else {
        Err(String::from("Attempting to eval expression as statement"))
    }
}

fn eval_program_with_env(
    program: parse::ast::Program,
    environment: &Environment,
) -> Result<Object, String> {
    let mut result = Ok(NULL);
    for statement in program.statements {
        let inner_result = eval_statement(statement, environment)?;
        let returning = inner_result.is_return;
        result = Ok(inner_result.result);
        if returning {
            break;
        }
    }
    result
}

pub fn eval_program(
    program: parse::ast::Program,
    environment: Option<&Environment>,
) -> Result<Object, String> {
    if let Some(e) = environment {
        eval_program_with_env(program, e)
    } else {
        let env = Environment::new();
        eval_program_with_env(program, &env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex;

    fn check_eval(input: &str) -> Result<Object, String> {
        let l = lex::Lexer::for_str(input);
        let mut p = parse::Parser::for_lexer(l);
        let prog = p.parse()?;
        eval_program(prog, None)
    }

    fn check_integer_object(obj: Object, expected: Option<i64>) {
        match obj.get_type() {
            ObjectType::Integer(i) => {
                if let Some(exp) = expected {
                    assert_eq!(&i, &exp)
                } else {
                    assert!(false, "Expected NULL, got {}", i)
                }
            }
            ObjectType::Null => match expected {
                Some(i) => assert!(false, "Expected {}, got NULL", i),
                None => assert!(true),
            },
            _ => assert!(false, "object was not integer type"),
        }
    }

    fn check_boolean_object(obj: Object, expected: &bool) {
        if let ObjectType::Boolean(b) = obj.get_type() {
            assert_eq!(&b, expected);
        } else {
            assert!(false, "object was not boolean type");
        }
    }

    mod test_bool {
        use super::*;
        macro_rules! bool_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    check_boolean_object(check_eval(input).unwrap(), &expected);
                })*

        }
    }

        bool_tests! {
            littrue: ("true", true),
            litfalse: ("false", false),
            onelttwo: ("1 < 2", true),
            onegttwo: ("1 > 2", false),
            oneltone: ("1 < 1", false),
            onegtone: ("1 > 1", false),
            oneeqone: ("1 == 1", true),
            oneneqone: ("1 != 1", false),
            oneeqtwo: ("1 == 2", false),
            oneneqtwo: ("1 != 2", true),
            trueeqtrue: ("true == true", true),
            falseeqfalse: ("false == false", true),
            trueeqfalse: ("true == false", false),
            trueneqfalse: ("true != false", true),
            falseneqtrue: ("false != true", true),
            onelttwoeqtrue: ("(1 < 2) == true", true),
            onelttwoeqfalse: ("(1 < 2) == false", false),
            onegttwoeqtrue: ("(1 > 2) == true", false),
            onegttwoeqfalse: ("(1 > 2) == false", true),
        }
    }

    mod test_int {
        use super::*;
        macro_rules! int_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    check_integer_object(check_eval(input).unwrap(), Some(expected));
                })*

        }
    }

        int_tests! {
            five: ("5", 5),
            ten: ("10", 10),
            minusfive: ("-5", -5),
            minusten: ("-10", -10),
            addsub: ("5 + 5 + 5 + 5 - 10 + 1", 11),
            mult: ("2 * 2 * 2 * 2 * 2", 32),
            addsubneg: ("-50 + 100 + -50", 0),
            multadd: ("5 * 2 + 10", 20),
            addmult: ("5 + 2 * 10", 25),
            addmultneg: ("20 + 2 * -10", 0),
            divmultadd: ("22 / 2 * 3 + 9", 42),
            multbrack: ("3 * (9 + 1)", 30),
            multmultadd: ("3 * 3 * 3 + 10", 37),
            multbrackadd: ("3 * (3 * 3) + 10", 37),
            bigbrack: ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        }
    }

    mod test_bang {
        use super::*;
        macro_rules! bang_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    check_boolean_object(check_eval(input).unwrap(), &expected);
                })*

        }
    }

        bang_tests! {
            bangtrue: ("!true", false),
            bangfalse: ("!false", true),
            bangfive: ("!5", false),
            bangbangtrue: ("!!true", true),
            bangbangfalse: ("!!false", false),
            bangbangfive: ("!!5", true),
        }
    }

    mod test_conditional {
        use super::*;

        macro_rules! cond_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    check_integer_object(check_eval(input).unwrap(), expected);
                })*

        }
    }

        cond_tests! {
            iftrue: ("if (true) { 10 }", Some(10)),
            iffalse: ("if (false) { 10 }", None),
            ifone: ("if (1) { 10 }", Some(10)),
            ifonelttwo: ("if (1<2) { 10 }", Some(10)),
            ifonegttwo: ("if (1>2) { 10 }", None),
            ifelseonegttwo: ("if (1>2) { 10 } else { 20 }", Some(20)),
            ifelseonelttwo: ("if (1<2) { 10 } else { 20 }", Some(10)),
        }
    }

    mod test_returns {
        use super::*;

        macro_rules! return_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    check_integer_object(check_eval(input).unwrap(), expected);
                })*

        }
    }

        return_tests! {
            ret10: ("return 10", Some(10)),
            ret10not9: ("return 10; 9;", Some(10)),
            ret2x5not9: ("return 2*5; 9;", Some(10)),
            ret2x5not9or9: ("9; return 2*5; 9;", Some(10)),
            nested: ("if(10>1) { if (10>1) { return 10; } } return 9;", Some(10)),
        }
    }

    mod test_errors {
        use super::*;

        #[allow(unused)]
        fn check_error_output(result: Result<Object, String>, expected: &str) {
            if let Err(s) = result {
                assert_eq!(s, expected)
            } else {
                assert!(false, "Expecting error, but got result")
            }
        }

        macro_rules! error_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    check_error_output(check_eval(input), expected);
                })*

        }
    }
        error_tests! {
            intaddbool: ("5+true;", "type mismatch: INTEGER + BOOLEAN"),
            intaddboolfirst: ("5+true; return 9;", "type mismatch: INTEGER + BOOLEAN"),
            minusbool: ("-true", "undefined prefix operation: -BOOLEAN"),
            boolplusbool: ("true+false", "undefined operation: BOOLEAN + BOOLEAN"),
            boolplusboolfirst: ("true+false; return 10;", "undefined operation: BOOLEAN + BOOLEAN"),
            boolplusboolcond: ("if (10 > 1) {true+false}; return 10;", "undefined operation: BOOLEAN + BOOLEAN"),
            boolplusboolnestedcond: ("if (20 > 10) { if (10 > 1) {true+false}}; return 10;", "undefined operation: BOOLEAN + BOOLEAN"),
            undefinedident: ("foobar", "undefined identifier: foobar"),
        }
    }

    mod test_let {
        use super::*;

        macro_rules! error_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    check_integer_object(check_eval(input).unwrap(), expected);
                })*

        }
    }
        error_tests! {
            assignfive: ("let a = 5; a;", Some(5)),
            assignfivexfive: ("let a = 5 * 5; a;", Some(25)),
            assignbtoa: ("let a = 6; let b = a; b;", Some(6)),
            assignexpr: ("let a = 5; let b = a; let c = a + b + 5; c;", Some(15)),
        }
    }
}
