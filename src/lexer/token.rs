#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Illegal,               // illegal token
    Eof,                   // end of file
    Identifier(String),    // identifier
    IntegerLiteral(i64),   // int
    FloatLiteral(f64),     // float
    StringLiteral(String), // string("" or '')
    Comma,                 // ,
    Semi,                  // ;
    Dot,                   // .
    Colon,                 // :
    Label,                 // ::
    LCurly,                // {
    RCurly,                // }
    LBracket,              // [
    RBracket,              // ]
    LParen,                // (
    RParen,                // )
    Assign,                // =
    Or,                    // || | or | Or | OR
    And,                   // && | and | And | AND
    Eq,                    // ==
    NotEq,                 // !=
    Gt,                    // >
    GtEq,                  // >=
    Lt,                    // <
    LtEq,                  // <=
    Plus,                  // +
    Minus,                 // -
    Mul,                   // *
    Div,                   // /
    Mod,                   // %
    Not,                   // not | Not | NOT | !
    False,                 // false | False | False
    True,                  // true | True | TRUE
    Null,                  // null | Null | NULL
}

impl Token {
    pub fn lookup_identifier(ident: &str) -> Self {
        match ident {
            "or" | "Or" | "OR" => Token::Or,
            "and" | "And" | "AND" => Token::And,
            "not" | "Not" | "NOT" => Token::Not,
            "true" | "True" | "TRUE" => Token::True,
            "false" | "False" | "FALSE" => Token::False,
            "null" | "Null" | "NULL" => Token::Null,
            _ => Token::Identifier(ident.to_string()),
        }
    }
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Precedence {
    Lowest,
    Or,      // or
    And,     // and
    Compare, // == != > >= < <=
    Sum,     // + -
    Product, // * / %
    Not,     // not
    Call,    // call
    Index,   // index
}

impl Token {
    pub fn precedence(&self) -> Precedence {
        match self {
            Token::LBracket => Precedence::Index,
            Token::LParen => Precedence::Call,
            Token::Not => Precedence::Not,
            Token::Mul | Token::Div => Precedence::Product,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Eq | Token::NotEq | Token::Gt | Token::GtEq | Token::Lt | Token::LtEq => {
                Precedence::Compare
            }
            Token::And => Precedence::And,
            Token::Or => Precedence::Or,
            _ => Precedence::Lowest,
        }
    }
}
