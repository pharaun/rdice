use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Token {
    Str(String),
    Num(u32),
}

// Lexer
pub struct Lexer<'a> {
    input_iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer { input_iter: input.chars().peekable() }
    }

    fn discard_char(&mut self) {
        let _ = self.input_iter.next();
    }

    fn read_char(&mut self) -> Option<char> {
        self.input_iter.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input_iter.peek()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek_char() {
            if c.is_whitespace() {
                self.discard_char();
            } else {
                break;
            }
        }
    }

    fn read_ident(&mut self, c: char) -> String {
        let mut ident = String::new();
        ident.push(c);

        while let Some(&c) = self.peek_char() {
            if c.is_alphabetic() {
                ident.push(self.read_char().unwrap());
            } else {
                break;
            }
        }
        ident
    }

    fn read_digits(&mut self, c: char, radix: u32) -> u32 {
        let mut digits = String::new();
        digits.push(c);

        while let Some(&c) = self.peek_char() {
            if c.is_digit(radix) {
                digits.push(self.read_char().unwrap());
            } else {
                break;
            }
        }
        u32::from_str_radix(&digits, radix).unwrap()
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if let Some(c) = self.read_char() {
            if c.is_alphabetic() {
                Some(Token::Str(self.read_ident(c)))
            } else if c.is_digit(10) {
                Some(Token::Num(self.read_digits(c, 10)))
            } else {
                println!("Unknown characters: {:?}", c);
                panic!("Isn't an alphabetic or digits")
            }
        } else {
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.next_token()
    }
}

#[cfg(test)]
pub mod lexer_token {
    use super::*;

    #[test]
    fn single_dice() {
        let input = "10d10";
        let mut lexer = Lexer::new(input);

        let expected = vec![
            Some(Token::Num(10)),
            Some(Token::Str("d".to_string())),
            Some(Token::Num(10)),
            None,
        ];

        // Assert
        for e in expected.iter() {
            let t = &lexer.next_token();
            println!("expected {:?}, lexed {:?} ", e, t);
            assert_eq!(e, t);
        }
    }
}
