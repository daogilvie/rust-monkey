use crate::lex::{Token, TokenType};
use crate::object::*;
use crate::parse;
use crate::parse::ast::{ExpressionKind, ExpressionNode, StatementKind, StatementNode};

fn eval_prefix_expression(operator: &Token, right: Object) -> Result<Object, String> {
    match &operator.token_type {
        TokenType::BANG => match right.get_type() {
            ObjectType::Integer(i) => {
                if i == 0 {
                    Ok(TRUE)
                } else {
                    Ok(FALSE)
                }
            }
            ObjectType::Boolean(b) => {
                if b {
                    Ok(FALSE)
                } else {
                    Ok(TRUE)
                }
            }
            ObjectType::Null => Ok(TRUE),
        },
        TokenType::MINUS => match right.get_type() {
            ObjectType::Integer(i) => Ok(Object::with_type(ObjectType::Integer(-i))),
            _ => Err(format!(
                "{:?} not supported on rhs of MINUS",
                right.get_type()
            )),
        },
        _ => Err(format!("Cannot eval prefix from {}", operator)),
    }
}

pub fn eval_expression(expr: &ExpressionNode) -> Result<Object, String> {
    match &expr.kind {
        ExpressionKind::IntegerLiteral { value } => {
            Ok(Object::with_type(ObjectType::Integer(*value)))
        }
        ExpressionKind::BooleanLiteral { value } => {
            if *value {
                Ok(TRUE)
            } else {
                Ok(FALSE)
            }
        }
        ExpressionKind::PrefixExpression { operator, right } => {
            let eval_r = eval_expression(&*right)?;
            eval_prefix_expression(&operator, eval_r)
        }
        _ => Err(format!("Cannot eval '{:?}'", expr.kind)),
    }
}

pub fn eval_statement(statement: StatementNode) -> Result<Object, String> {
    match &statement.kind {
        StatementKind::Expression { expression } => eval_expression(expression),
        _ => Err(format!("Cannot eval statement kind {:?}", statement.kind)),
    }
}

pub fn eval_program(mut program: parse::ast::Program) -> Result<Object, String> {
    // Right now we just parse the statement, this will obviously change.
    if let Some(stmt) = program.statements.pop() {
        eval_statement(stmt)
    } else {
        Err(String::from("Your program has no statements"))
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
        eval_program(prog)
    }

    fn check_integer_object(obj: Object, expected: &i64) {
        if let ObjectType::Integer(i) = obj.get_type() {
            assert_eq!(&i, expected)
        } else {
            assert!(false, "object was not integer type")
        }
    }

    fn check_boolean_object(obj: Object, expected: &bool) {
        if let ObjectType::Boolean(b) = obj.get_type() {
            assert_eq!(&b, expected);
        } else {
            assert!(false, "object was not boolean type");
        }
    }

    #[test]
    fn test_eval_boolean_literals() {
        let cases = vec![("true", true), ("false", false)];
        for (expr, value) in cases {
            check_boolean_object(check_eval(expr).unwrap(), &value);
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
                    check_integer_object(check_eval(input).unwrap(), &expected);
                })*

        }
    }

        int_tests! {
            five: ("5", 5),
            ten: ("10", 10),
            minusfive: ("-5", -5),
            minusten: ("-10", -10),
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
}
