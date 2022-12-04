mod error;
mod object;

use crate::parser::ast::*;
use anyhow::Result;
use error::RuntimeError;
use object::{ObjType, Object};
use std::any::Any;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Evaluator;

type EvalResult = Result<Object, RuntimeError>;

const TRUE: Object = Object::Bool(true);
const FALSE: Object = Object::Bool(false);
const NULL: Object = Object::Null;

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&mut self, program: Program) -> EvalResult {
        let mut result = NULL;
        for stmt in program.stmts {
            result = self.eval_stmt(&*stmt)?;
            if let Object::Return(obj) = result {
                return Ok(*obj);
            }
        }
        Ok(result)
    }

    fn eval_stmt(&mut self, stmt: &dyn Stmt) -> EvalResult {
        if let Some(expr_stmt) = stmt.downcast_ref::<ExprStmt>() {
            Ok(self.eval_expr_stmt(expr_stmt)?)
        } else {
            panic!("err")
        }
    }

    fn eval_expr_stmt(&mut self, stmt: &ExprStmt) -> EvalResult {
        self.eval_expr(&*stmt.expr)
    }

    fn eval_expr(&mut self, expr: &dyn Expr) -> EvalResult {
        if let Some(expr) = expr.downcast_ref::<BinaryExpression>() {
            self.eval_binary_expr(expr)
        } else if let Some(expr) = expr.downcast_ref::<UnaryExpression>() {
            self.eval_unary_expr(expr)
        } else if let Some(expr) = expr.downcast_ref::<ParenExpression>() {
            self.eval_paren_expr(expr)
        } else if let Some(ident) = expr.downcast_ref::<Identifier>() {
            self.eval_identifier(ident)
        } else if let Some(call) = expr.downcast_ref::<FunctionCall>() {
            self.eval_function_call(call)
        } else if let Some(arr) = expr.downcast_ref::<ArrayLiteral>() {
            self.eval_array(arr)
        } else if let Some(value) = expr.downcast_ref::<IntegerLiteral>() {
            self.eval_int(value)
        } else if let Some(value) = expr.downcast_ref::<FloatLiteral>() {
            self.eval_float(value)
        } else if let Some(value) = expr.downcast_ref::<BooleanLiteral>() {
            self.eval_bool(value)
        } else if let Some(value) = expr.downcast_ref::<StringLiteral>() {
            self.eval_string(value)
        } else {
            panic!("err")
        }
    }

    fn eval_binary_expr(&mut self, expr: &BinaryExpression) -> EvalResult {
        let mut left = self.eval_expr(&*expr.left)?;
        let mut right = self.eval_expr(&*expr.right)?;
        let op = expr.op.clone();

        match (&left, &right) {
            (Object::Int(l), Object::Int(r)) => self.eval_integer_binary_operations(op, *l, *r),
            (Object::Int(l), Object::Float(r)) => {
                self.eval_float_binary_operations(op, *l as f64, *r)
            }
            (Object::Float(l), Object::Int(r)) => {
                self.eval_float_binary_operations(op, *l, *r as f64)
            }
            (Object::Float(l), Object::Float(r)) => self.eval_float_binary_operations(op, *l, *r),
            (Object::String(l), Object::String(r)) => {
                self.eval_string_binary_operations(op, l.clone(), r.clone())
            }
            _ => match op {
                BinaryOperator::Eq => Ok(Object::Bool(left == right)),
                BinaryOperator::NotEq => Ok(Object::Bool(left != right)),
                _ => Err(RuntimeError::InvalidInfixOperands {
                    op,
                    left: left.typ(),
                    right: right.typ(),
                }),
            },
        }
    }

    fn eval_integer_binary_operations(
        &mut self,
        op: BinaryOperator,
        left: i64,
        right: i64,
    ) -> EvalResult {
        match op {
            BinaryOperator::Eq => Ok(Object::Bool(left == right)),
            BinaryOperator::NotEq => Ok(Object::Bool(left != right)),
            BinaryOperator::Gt => Ok(Object::Bool(left > right)),
            BinaryOperator::GtEq => Ok(Object::Bool(left >= right)),
            BinaryOperator::Lt => Ok(Object::Bool(left < right)),
            BinaryOperator::LtEq => Ok(Object::Bool(left <= right)),
            BinaryOperator::Add => Ok(Object::Int(left + right)),
            BinaryOperator::Sub => Ok(Object::Int(left - right)),
            BinaryOperator::Mul => Ok(Object::Int(left * right)),
            BinaryOperator::Div => {
                if right == 0 {
                    Err(RuntimeError::IntegerDivisionByZero { op, left, right })
                } else if left % right == 0 {
                    Ok(Object::Int(left / right))
                } else {
                    Ok(Object::Float(left as f64 / right as f64))
                }
            }
            BinaryOperator::Mod => {
                if right == 0 {
                    Err(RuntimeError::IntegerDivisionByZero { op, left, right })
                } else {
                    Ok(Object::Int(left % right))
                }
            }
            _ => Err(RuntimeError::InvalidInfixOperands {
                op,
                left: ObjType::Int,
                right: ObjType::Int,
            }),
        }
    }

    fn eval_float_binary_operations(
        &mut self,
        op: BinaryOperator,
        left: f64,
        right: f64,
    ) -> EvalResult {
        match op {
            BinaryOperator::Eq => Ok(Object::Bool(left == right)),
            BinaryOperator::NotEq => Ok(Object::Bool(left != right)),
            BinaryOperator::Gt => Ok(Object::Bool(left > right)),
            BinaryOperator::GtEq => Ok(Object::Bool(left >= right)),
            BinaryOperator::Lt => Ok(Object::Bool(left < right)),
            BinaryOperator::LtEq => Ok(Object::Bool(left <= right)),
            BinaryOperator::Add => Ok(Object::Float(left + right)),
            BinaryOperator::Sub => Ok(Object::Float(left - right)),
            BinaryOperator::Mul => Ok(Object::Float(left * right)),
            BinaryOperator::Div => {
                if right == 0.0 {
                    Err(RuntimeError::FloatDivisionByZero { op, left, right })
                } else {
                    Ok(Object::Float(left / right))
                }
            }
            _ => Err(RuntimeError::InvalidInfixOperands {
                op,
                left: ObjType::Float,
                right: ObjType::Float,
            }),
        }
    }

    fn eval_string_binary_operations(
        &mut self,
        op: BinaryOperator,
        left: String,
        right: String,
    ) -> EvalResult {
        match op {
            BinaryOperator::Eq => Ok(Object::Bool(left == right)),
            BinaryOperator::NotEq => Ok(Object::Bool(left != right)),
            BinaryOperator::Gt => Ok(Object::Bool(left > right)),
            BinaryOperator::GtEq => Ok(Object::Bool(left >= right)),
            BinaryOperator::Lt => Ok(Object::Bool(left < right)),
            BinaryOperator::LtEq => Ok(Object::Bool(left <= right)),
            BinaryOperator::Add => Ok(Object::String(left + &*right)),
            _ => Err(RuntimeError::InvalidInfixOperands {
                op,
                left: ObjType::String,
                right: ObjType::String,
            }),
        }
    }

    fn eval_unary_expr(&mut self, expr: &UnaryExpression) -> EvalResult {
        let mut obj = self.eval_expr(&*expr.expr)?;
        match expr.op {
            UnaryOperator::Plus => match obj {
                Object::Int(_) | Object::Float(_) => Ok(obj),
                _ => Err(RuntimeError::InvalidPrefixOperand {
                    op: UnaryOperator::Plus,
                    right: obj.typ(),
                }),
            },
            UnaryOperator::Minus => match obj {
                Object::Int(int) => Ok(Object::Int(-int)),
                Object::Float(float) => Ok(Object::Float(-float)),
                _ => Err(RuntimeError::InvalidPrefixOperand {
                    op: UnaryOperator::Plus,
                    right: obj.typ(),
                }),
            },
            UnaryOperator::Not => match obj {
                Object::Bool(value) => Ok(Object::Bool(!value)),
                _ => Err(RuntimeError::InvalidPrefixOperand {
                    op: UnaryOperator::Not,
                    right: obj.typ(),
                }),
            },
        }
    }

    fn eval_paren_expr(&mut self, expr: &ParenExpression) -> EvalResult {
        self.eval_expr(&*expr.expr)
    }

    fn eval_identifier(&mut self, ident: &Identifier) -> EvalResult {
        todo!()
    }

    fn eval_int(&mut self, int: &IntegerLiteral) -> EvalResult {
        Ok(Object::Int(int.0))
    }

    fn eval_float(&mut self, float: &FloatLiteral) -> EvalResult {
        Ok(Object::Float(float.0))
    }

    fn eval_bool(&mut self, bool: &BooleanLiteral) -> EvalResult {
        Ok(Object::Bool(bool.0))
    }

    fn eval_string(&mut self, str: &StringLiteral) -> EvalResult {
        Ok(Object::String(str.0.clone()))
    }

    fn eval_function_call(&mut self, call: &FunctionCall) -> EvalResult {
        todo!()
    }

    fn eval_array(&mut self, arr: &ArrayLiteral) -> EvalResult {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval() {
        let lexer = Lexer::new("1 + 2 + 3");
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        let mut evaluator = Evaluator::new();
        let obj = evaluator.eval(program).unwrap();
        assert_eq!(obj.to_string(), "6")
    }
}
