use crate::object::{Object, ObjectType};
use crate::parse;
use crate::parse::ast::{ExpressionKind, ExpressionNode, StatementKind, StatementNode};

pub fn eval_expression(expr: &ExpressionNode) -> Result<Object, String> {
    match expr.kind {
        ExpressionKind::IntegerLiteral { value } => {
            Ok(Object::with_type(ObjectType::Integer(value)))
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
        let mut l = lex::Lexer::for_str(input);
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

    #[test]
    fn test_eval_integer_expressions() {
        let cases = vec![("5", 5), ("10", 10)];
        for (expr, value) in cases {
            check_integer_object(check_eval(expr).unwrap(), &value);
        }
    }
}
