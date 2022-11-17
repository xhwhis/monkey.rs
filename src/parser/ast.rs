use crate::lexer::token::Token;
use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub struct Identifier(pub String);

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self(value.to_owned())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IntegerLiteral(pub i64);

impl From<i64> for IntegerLiteral {
    fn from(value: i64) -> Self {
        Self(value)
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct FloatLiteral(pub f64);

impl From<f64> for FloatLiteral {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

impl fmt::Display for FloatLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct BooleanLiteral(pub bool);

impl From<bool> for BooleanLiteral {
    fn from(value: bool) -> Self {
        Self(value)
    }
}

impl fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct StringLiteral(pub String);

impl From<&str> for StringLiteral {
    fn from(value: &str) -> Self {
        Self(value.to_owned())
    }
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ParenExpression {
    expr: Box<Expr>,
}

impl ParenExpression {
    pub fn new(expr: Expr) -> Self {
        Self {
            expr: Box::new(expr),
        }
    }
}

impl fmt::Display for ParenExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.expr)
    }
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

#[derive(PartialEq, Clone, Debug)]
pub struct UnaryExpression {
    pub op: UnaryOperator,
    pub expr: Box<Expr>,
}

impl UnaryExpression {
    pub fn new(op: UnaryOperator, expr: Expr) -> Self {
        Self {
            op,
            expr: Box::new(expr),
        }
    }
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.op, self.expr)
    }
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

#[derive(PartialEq, Clone, Debug)]
pub struct BinaryExpression {
    pub op: BinaryOperator,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl BinaryExpression {
    pub fn new(op: BinaryOperator, left: Expr, right: Expr) -> Self {
        Self {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

impl fmt::Display for BinaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunctionCall {
    func: Box<Expr>,
    args: Vec<Expr>,
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

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayLiteral {
    elems: Vec<Expr>,
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

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    BooleanLiteral(BooleanLiteral),
    StringLiteral(StringLiteral),
    ParenExpression(ParenExpression),
    UnaryExpression(UnaryExpression),
    BinaryExpression(BinaryExpression),
    FunctionCall(FunctionCall),
    ArrayLiteral(ArrayLiteral),
}

macro_rules! impl_from {
    ($enum:ident, $ident:ident) => {
        impl From<$ident> for $enum {
            fn from(value: $ident) -> Self {
                $enum::$ident(value)
            }
        }
    };
}

impl_from!(Expr, Identifier);
impl_from!(Expr, IntegerLiteral);
impl_from!(Expr, FloatLiteral);
impl_from!(Expr, BooleanLiteral);
impl_from!(Expr, StringLiteral);
impl_from!(Expr, ParenExpression);
impl_from!(Expr, UnaryExpression);
impl_from!(Expr, BinaryExpression);
impl_from!(Expr, FunctionCall);
impl_from!(Expr, ArrayLiteral);

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Identifier(expr) => write!(f, "{}", expr),
            Expr::IntegerLiteral(expr) => write!(f, "{}", expr),
            Expr::FloatLiteral(expr) => write!(f, "{}", expr),
            Expr::BooleanLiteral(expr) => write!(f, "{}", expr),
            Expr::StringLiteral(expr) => write!(f, "{}", expr),
            Expr::ParenExpression(expr) => write!(f, "{}", expr),
            Expr::UnaryExpression(expr) => write!(f, "{}", expr),
            Expr::BinaryExpression(expr) => write!(f, "{}", expr),
            Expr::FunctionCall(expr) => write!(f, "{}", expr),
            Expr::ArrayLiteral(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

impl fmt::Display for ExprStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    ExprStmt(ExprStmt),
}

impl_from!(Stmt, ExprStmt);

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::ExprStmt(stmt) => write!(f, "{}", stmt),
        }
    }
}

#[derive(PartialEq, Clone, Default, Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

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

#[derive(PartialEq, Clone, Debug)]
pub enum Node {
    Expr(Expr),
    Stmt(Stmt),
    Program(Program),
}

impl_from!(Node, Expr);
impl_from!(Node, Stmt);
impl_from!(Node, Program);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_expr_display() {
        let expr = Expr::from(BinaryExpression {
            op: BinaryOperator::Add,
            left: Box::new(Expr::from(Identifier::from("a"))),
            right: Box::new(Expr::from(IntegerLiteral::from(1))),
        });
        assert_eq!(expr.to_string(), "a + 1");
    }

    #[test]
    fn func_call_display() {
        let expr = Expr::from(FunctionCall {
            func: Box::new(Expr::from(Identifier::from("func"))),
            args: vec![Expr::from(BinaryExpression {
                op: BinaryOperator::Sub,
                left: Box::new(Expr::from(FunctionCall {
                    func: Box::new(Expr::from(Identifier::from("func1"))),
                    args: vec![
                        Expr::from(StringLiteral::from("hello")),
                        Expr::from(BooleanLiteral::from(true)),
                    ],
                })),
                right: Box::new(Expr::from(FloatLiteral(1.2))),
            })],
        });
        assert_eq!(expr.to_string(), "func(func1(\"hello\", true) - 1.2)")
    }

    #[test]
    fn array_literal_display() {
        let expr = Expr::from(ArrayLiteral {
            elems: vec![
                Expr::from(Identifier::from("a")),
                Expr::from(IntegerLiteral::from(1)),
                Expr::from(FloatLiteral::from(1.1)),
                Expr::from(BooleanLiteral::from(false)),
                Expr::from(StringLiteral::from("hi")),
            ],
        });
        assert_eq!(expr.to_string(), "[a, 1, 1.1, false, \"hi\"]")
    }
}
