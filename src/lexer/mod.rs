pub mod token;

use std::iter::Peekable;
use std::str::Chars;
use token::*;

#[derive(Debug)]
pub struct Lexer<'a> {
    src: Peekable<Chars<'a>>,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut lexer = Lexer {
            src: src.chars().peekable(),
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        macro_rules! peek_char {
            ($c:literal, $token:ident, $other:ident) => {
                if self.src.peek().unwrap().eq(&$c) {
                    self.read_char();
                    Token::$token
                } else {
                    Token::$other
                }
            };
        }

        let token = match self.ch {
            '=' => peek_char!('=', Eq, Assign),
            ';' => Token::Semi,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            ':' => peek_char!(':', Label, Colon),
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => peek_char!('=', NotEq, Not),
            '/' => Token::Div,
            '*' => Token::Mul,
            '%' => Token::Mod,
            '<' => peek_char!('=', LtEq, Lt),
            '>' => peek_char!('=', GtEq, Gt),
            '{' => Token::LCurly,
            '}' => Token::RCurly,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '.' => Token::Dot,
            '|' => peek_char!('|', Or, Illegal),
            '&' => peek_char!('&', And, Illegal),
            '\0' => Token::EOF,

            'A'..='Z' | 'a'..='z' | '_' => {
                let lit = self.read_identifier();
                return Token::from(lit.as_str());
            }

            '0'..='9' => self.read_number(),

            '"' => self.read_string(),

            _ => Token::Illegal,
        };
        self.read_char();
        token
    }

    fn read_char(&mut self) {
        self.ch = self.src.next().unwrap_or('\0')
    }

    fn char_is(&self, c: char) -> bool {
        self.ch.eq(&c)
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> Token {
        let mut number = String::new();
        let is_float = false;
        loop {
            if self.char_is('\0') {
                break;
            }

            if self.ch.is_ascii_digit() {
                number.push(self.ch);
                self.read_char();
            } else if self.char_is('.') {
                if is_float {
                    break;
                }
                number.push(self.ch);
                self.read_char();
            } else {
                break;
            }
        }
        if is_float {
            Token::FloatLit(number.parse::<f64>().unwrap())
        } else {
            Token::IntLit(number.parse::<i64>().unwrap())
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while self.ch.is_alphabetic() || self.ch == '_' || self.ch.is_ascii_digit() {
            ident.push(self.ch);
            self.read_char();
        }
        ident
    }

    fn read_string(&mut self) -> Token {
        let mut string = String::new();
        self.read_char();
        while self.ch != '"' && self.ch != '\0' {
            string.push(self.ch);
            self.read_char();
        }
        Token::StringLit(string)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let src = "get_pid() != 0 AND time() > 1667923200 OR get_uid() == \"1\"";
        let mut lexer = Lexer::new(src);
        let expected_tokens = [
            Token::Identifier("get_pid".to_owned()),
            Token::LParen,
            Token::RParen,
            Token::NotEq,
            Token::IntLit(0),
            Token::And,
            Token::Identifier("time".to_owned()),
            Token::LParen,
            Token::RParen,
            Token::Gt,
            Token::IntLit(1667923200),
            Token::Or,
            Token::Identifier("get_uid".to_owned()),
            Token::LParen,
            Token::RParen,
            Token::Eq,
            Token::StringLit("1".to_owned()),
        ];
        for expected in expected_tokens {
            let actual = lexer.next_token();
            assert_eq!(expected, actual);
        }
    }
}
