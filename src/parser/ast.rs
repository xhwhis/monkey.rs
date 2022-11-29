use crate::lexer::token::Token;
use std::fmt;

pub trait Node: fmt::Display {
    fn is_node(&self);
}

pub trait Stmt: Node {
    fn is_stmt(&self);
}

pub trait Expr: Node {
    fn is_expr(&self);
}

trait BasicLiteral: Expr {
    fn is_basic_literal(&self);
}

pub struct Program {
    pub stmts: Vec<Box<dyn Stmt>>,
}

pub struct ExprStmt {
    pub expr: Box<dyn Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum BinaryOperator {
    Or,    // or
    And,   // and
    Eq,    // ==
    NotEq, // !=
    Gt,    // >
    GtEq,  // >=
    Lt,    // <
    LtEq,  // <=
    Add,   // +
    Sub,   // -
    Mul,   // *
    Div,   // /
    Mod,   // %
}

impl From<&Token> for BinaryOperator {
    fn from(value: &Token) -> Self {
        match value {
            Token::Or => BinaryOperator::Or,
            Token::And => BinaryOperator::And,
            Token::Eq => BinaryOperator::Eq,
            Token::NotEq => BinaryOperator::NotEq,
            Token::Gt => BinaryOperator::Gt,
            Token::GtEq => BinaryOperator::GtEq,
            Token::Lt => BinaryOperator::Lt,
            Token::LtEq => BinaryOperator::LtEq,
            Token::Plus => BinaryOperator::Add,
            Token::Minus => BinaryOperator::Sub,
            Token::Mul => BinaryOperator::Mul,
            Token::Div => BinaryOperator::Div,
            Token::Mod => BinaryOperator::Mod,
            _ => panic!("err"),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Eq => write!(f, "=="),
            BinaryOperator::NotEq => write!(f, "!="),
            BinaryOperator::Gt => write!(f, ">"),
            BinaryOperator::GtEq => write!(f, ">="),
            BinaryOperator::Lt => write!(f, "<"),
            BinaryOperator::LtEq => write!(f, "<="),
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Mod => write!(f, "%"),
        }
    }
}

pub struct BinaryExpression {
    pub op: BinaryOperator,
    pub left: Box<dyn Expr>,
    pub right: Box<dyn Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum UnaryOperator {
    Plus,  // +
    Minus, // -
    Not,   // !
}

impl From<&Token> for UnaryOperator {
    fn from(value: &Token) -> Self {
        match value {
            Token::Plus => UnaryOperator::Plus,
            Token::Minus => UnaryOperator::Minus,
            Token::Not => UnaryOperator::Not,
            _ => panic!("err"),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Plus => write!(f, "+"),
            UnaryOperator::Minus => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

pub struct UnaryExpression {
    pub op: UnaryOperator,
    pub expr: Box<dyn Expr>,
}

pub struct ParenExpression {
    pub expr: Box<dyn Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Identifier(pub String);

pub struct FunctionCall {
    func: Box<dyn Expr>,
    args: Vec<Box<dyn Expr>>,
}

pub struct ArrayLiteral {
    elems: Vec<Box<dyn Expr>>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct IntegerLiteral(pub i64);

#[derive(PartialEq, Clone, Debug)]
pub struct FloatLiteral(pub f64);

#[derive(PartialEq, Clone, Debug)]
pub struct BooleanLiteral(pub bool);

#[derive(PartialEq, Clone, Debug)]
pub struct StringLiteral(pub String);

macro_rules! node_impl {
    ($node:ident) => {
        impl Node for $node {
            fn is_node(&self) {}
        }
    };
}

node_impl!(Program);
node_impl!(ExprStmt);
node_impl!(BinaryExpression);
node_impl!(UnaryExpression);
node_impl!(ParenExpression);
node_impl!(Identifier);
node_impl!(FunctionCall);
node_impl!(ArrayLiteral);
node_impl!(IntegerLiteral);
node_impl!(FloatLiteral);
node_impl!(BooleanLiteral);
node_impl!(StringLiteral);

macro_rules! stmt_impl {
    ($stmt:ident) => {
        impl Stmt for $stmt {
            fn is_stmt(&self) {}
        }
    };
}

stmt_impl!(ExprStmt);

macro_rules! expr_impl {
    ($expr:ident) => {
        impl Expr for $expr {
            fn is_expr(&self) {}
        }
    };
}

expr_impl!(BinaryExpression);
expr_impl!(UnaryExpression);
expr_impl!(ParenExpression);
expr_impl!(Identifier);
expr_impl!(FunctionCall);
expr_impl!(ArrayLiteral);
expr_impl!(IntegerLiteral);
expr_impl!(FloatLiteral);
expr_impl!(BooleanLiteral);
expr_impl!(StringLiteral);

macro_rules! basic_literal_impl {
    ($basic_literal:ident) => {
        impl BasicLiteral for $basic_literal {
            fn is_basic_literal(&self) {}
        }
    };
}

basic_literal_impl!(IntegerLiteral);
basic_literal_impl!(FloatLiteral);
basic_literal_impl!(BooleanLiteral);
basic_literal_impl!(StringLiteral);

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.stmts
                .iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl fmt::Display for ExprStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl fmt::Display for BinaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.op, self.expr)
    }
}

impl fmt::Display for ParenExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.expr)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.func,
            self.args
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.elems
                .iter()
                .map(|elem| elem.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for FloatLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_expr_display() {
        let expr = BinaryExpression {
            op: BinaryOperator::Add,
            left: Box::new(Identifier("a".to_owned())),
            right: Box::new(IntegerLiteral(1)),
        };
        assert_eq!(expr.to_string(), "a + 1");
    }

    #[test]
    fn func_call_display() {
        let expr = FunctionCall {
            func: Box::new(Identifier("func".to_owned())),
            args: vec![Box::new(BinaryExpression {
                op: BinaryOperator::Sub,
                left: Box::new(FunctionCall {
                    func: Box::new(Identifier("func1".to_owned())),
                    args: vec![
                        Box::new(StringLiteral("hello".to_owned())),
                        Box::new(BooleanLiteral(true)),
                    ],
                }),
                right: Box::new(FloatLiteral(1.2)),
            })],
        };
        assert_eq!(expr.to_string(), "func(func1(\"hello\", true) - 1.2)")
    }

    #[test]
    fn array_literal_display() {
        let expr = ArrayLiteral {
            elems: vec![
                Box::new(Identifier("a".to_owned())),
                Box::new(IntegerLiteral(1)),
                Box::new(FloatLiteral(1.1)),
                Box::new(BooleanLiteral(false)),
                Box::new(StringLiteral("hi".to_owned())),
            ],
        };
        assert_eq!(expr.to_string(), "[a, 1, 1.1, false, \"hi\"]")
    }
}
