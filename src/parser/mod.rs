pub mod ast;
pub mod error;

use crate::lexer::token::Token::StringLit;
use crate::lexer::token::*;
use crate::lexer::Lexer;
use crate::parser::ast::*;
use anyhow::Result;
use error::ParserError;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,
    next_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            token: Token::Illegal,
            next_token: Token::Illegal,
        };
        parser.next();
        parser.next();
        parser
    }

    fn next(&mut self) {
        self.token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut program = Program::default();
        while self.token != Token::EOF {
            let stmt = self.parse_stmt()?;
            program.stmts.push(stmt);
            self.next()
        }
        Ok(program)
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let stmt = Stmt::ExprStmt(self.parse_expr_stmt()?);
        Ok(stmt)
    }

    fn parse_expr_stmt(&mut self) -> Result<ExprStmt> {
        let expr_stmt = ExprStmt {
            expr: self.parse_expr()?,
        };
        Ok(expr_stmt)
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_binary_expr(Precedence::Lowest)
    }

    fn parse_binary_expr(&mut self, precedence: Precedence) -> Result<Expr> {
        let mut left = self.parse_unary_expr()?;
        loop {
            let cur_precedence = self.token.precedence();
            if cur_precedence <= precedence {
                return Ok(left);
            }
            let op = BinaryOperator::from(&self.token);
            self.next();
            let right = self.parse_binary_expr(cur_precedence)?;
            left = Expr::from(BinaryExpression::new(op, left, right));
        }
    }

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        if matches!(self.token, Token::Plus | Token::Minus | Token::Not) {
            let op = UnaryOperator::from(&self.token);
            self.next();
            let expr = self.parse_unary_expr()?;
            return Ok(ast::Expr::from(UnaryExpression::new(op, expr)));
        }
        self.parse_primary_expr()
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        match &self.token {
            Token::Identifier(_) => {
                if self.next_token == Token::LParen {
                    return self.parse_function_call();
                }
                self.parse_identifier()
            }
            Token::IntLit(_) => self.parse_integer_literal(),
            Token::FloatLit(_) => self.parse_float_literal(),
            Token::True | Token::False => self.parse_boolean_literal(),
            Token::StringLit(_) => self.parse_string_literal(),
            Token::LParen => {
                self.next();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(ast::Expr::from(ParenExpression::new(expr)))
            }
            token => Err(ParserError::InvalidPrefix(format!("{:?}", token)).into()),
        }
    }

    fn expect(&mut self, token: Token) -> Result<()> {
        if self.next_token == token {
            self.next();
            Ok(())
        } else {
            Err(
                ParserError::ExpectPeek(format!("{:?}", &token), format!("{:?}", &self.next_token))
                    .into(),
            )
        }
    }

    fn parse_identifier(&mut self) -> Result<Expr> {
        Ok(match self.token.clone() {
            Token::Identifier(value) => {
                self.next();
                Expr::from(Identifier(value))
            }
            _ => return Err(ParserError::ExpectIdentifier(format!("{:?}", self.token)).into()),
        })
    }

    fn parse_integer_literal(&mut self) -> Result<Expr> {
        Ok(match self.token {
            Token::IntLit(value) => {
                self.next();
                Expr::from(IntegerLiteral(value))
            }
            _ => return Err(ParserError::ExpectIdentifier(format!("{:?}", self.token)).into()),
        })
    }

    fn parse_float_literal(&mut self) -> Result<Expr> {
        Ok(match self.token {
            Token::FloatLit(value) => {
                self.next();
                Expr::from(FloatLiteral(value))
            }
            _ => return Err(ParserError::ExpectIdentifier(format!("{:?}", self.token)).into()),
        })
    }

    fn parse_boolean_literal(&mut self) -> Result<Expr> {
        Ok(match self.token {
            Token::False => {
                self.next();
                Expr::from(BooleanLiteral(false))
            }
            Token::True => {
                self.next();
                Expr::from(BooleanLiteral(true))
            }
            _ => return Err(ParserError::ExpectIdentifier(format!("{:?}", self.token)).into()),
        })
    }

    fn parse_string_literal(&mut self) -> Result<Expr> {
        Ok(match self.token.clone() {
            Token::StringLit(value) => {
                self.next();
                Expr::from(StringLiteral(value))
            }
            _ => return Err(ParserError::ExpectIdentifier(format!("{:?}", self.token)).into()),
        })
    }

    fn parse_function_call(&mut self) -> Result<Expr> {
        todo!()
    }

    fn parse_array_literal(&mut self) -> Result<Expr> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let lexer = Lexer::new("1 + 2");
        let mut parser = Parser::new(lexer);
        assert_eq!(parser.parse().unwrap().to_string(), "1 + 2");
    }
}
