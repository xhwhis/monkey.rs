#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Illegal,            // illegal token
    EOF,                // end of file
    Identifier(String), // identifier
    IntLit(i64),        // int
    FloatLit(f64),      // float
    StringLit(String),  // string("" or '')
    Comma,              // ,
    Semi,               // ;
    Dot,                // .
    Colon,              // :
    Label,              // ::
    LCurly,             // {
    RCurly,             // }
    LBracket,           // [
    RBracket,           // ]
    LParen,             // (
    RParen,             // )
    Assign,             // =
    Or,                 // || | or | Or | OR
    And,                // && | and | And | AND
    Eq,                 // ==
    NotEq,              // !=
    Gt,                 // >
    GtEq,               // >=
    Lt,                 // <
    LtEq,               // <=
    Plus,               // +
    Minus,              // -
    Mul,                // *
    Div,                // /
    Mod,                // %
    Not,                // not | Not | NOT | !
    False,              // false | False | False
    True,               // true | True | TRUE
    Null,               // null | Null | NULL
}

impl From<&str> for Token {
    fn from(value: &str) -> Self {
        match value {
            "or" | "Or" | "OR" => Token::Or,
            "and" | "And" | "AND" => Token::And,
            "not" | "Not" | "NOT" => Token::Not,
            "true" | "True" | "TRUE" => Token::True,
            "false" | "False" | "FALSE" => Token::False,
            "null" | "Null" | "NULL" => Token::Null,
            ident => Token::Identifier(ident.into()),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_identifier() {
        assert_eq!(Token::from("or"), Token::Or);
        assert_eq!(Token::from("and"), Token::And);
        assert_eq!(Token::from("not"), Token::Not);
        assert_eq!(Token::from("true"), Token::True);
        assert_eq!(Token::from("false"), Token::False);
        assert_eq!(Token::from("null"), Token::Null);
        assert_eq!(Token::from("var"), Token::Identifier("var".into()));
    }
}
