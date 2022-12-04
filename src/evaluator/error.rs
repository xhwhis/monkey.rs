use super::object::ObjType;
use crate::parser::ast;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("cannot perform `{op}` on `{right}`")]
    InvalidPrefixOperand {
        op: ast::UnaryOperator,
        right: ObjType,
    },
    #[error("cannot perform `{op}` between `{left}` and `{right}`")]
    InvalidInfixOperands {
        op: ast::BinaryOperator,
        left: ObjType,
        right: ObjType,
    },
    #[error("integer overflow occurred in the expression: `{x} {op} {y}`")]
    IntegerOverflow {
        op: ast::BinaryOperator,
        x: i64,
        y: i64,
    },
    #[error("integer division by zero occurred in the expression: `{left} {op} {right}`")]
    IntegerDivisionByZero {
        op: ast::BinaryOperator,
        left: i64,
        right: i64,
    },
    #[error("float division by zero occurred in the expression: `{left} {op} {right}`")]
    FloatDivisionByZero {
        op: ast::BinaryOperator,
        left: f64,
        right: f64,
    },
    #[error("variable not found: `{name}`")]
    VariableNotFound { name: String },
    #[error("not enough arguments to function call: got `{got}`, want `{expected}`")]
    NotEnoughArguments { expected: i32, got: i32 },
    #[error("not A function: {0}")]
    NotAFunction(ObjType),
    #[error("invalid function: {0}")]
    InvalidFunction(String),
    #[error("wrong argument type: got `{got}`")]
    WrongArgType { got: ObjType },
    #[error("index operator not supported between `{left}` and `{index}`")]
    IndexOperatorNotSupported { left: ObjType, index: ObjType },
    #[error("invalid index: {idx}")]
    InvalidIndex { idx: i64 },
}
