mod token;

pub use lexer::token::Token;

use lexer::token::TokenKind;
use std::io;
use std::str;
use util::Position;

struct Lexer<'a> {
    input: str::Lines<'a>,
    cur_line: Option<str::Chars<'a>>,
    cur_char: Option<char>,
    cur_position: Position,
    next_position: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lines = input.lines();
        let cur_line = lines.next();
        let mut lexer = Lexer {
            input: lines,
            cur_line: cur_line.map(|s| s.chars()),
            cur_char: None,
            cur_position: Position::new(),
            next_position: Position::new(),
        };
        lexer.next_char();
        lexer
    }

    fn next_char(&mut self) -> Option<char> {
        self.cur_position = self.next_position;
        self.cur_char = self.next_char_internal();
        self.cur_char
    }

    fn next_char_internal(&mut self) -> Option<char> {
        loop {
            if self.cur_line.is_none() {
                return None;
            }

            let char: Option<char> = self.cur_line.as_mut().unwrap().next();
            if char.is_none() {
                self.cur_line = self.input.next().map(|s| s.chars());
                self.next_position.next_line();
                continue;
            }

            self.next_position.next_col();
            return char;
        }
    }

    fn make_result(&self, token_kind: TokenKind, token_repr: String) -> Result<Token, Error> {
        Ok(Token::new(token_kind, token_repr, self.cur_position))
    }

    fn make_error(&self, error_kind: ErrorKind) -> Result<Token, Error> {
        Err(Error { kind: error_kind, position: self.cur_position })
    }

    pub fn next(&mut self) -> Result<Token, Error> {
        loop {
            if let Some(ch) = self.cur_char {
                if ch != ' ' { break }
            } else {
                return self.make_error(ErrorKind::Eof);
            }
        }

        let ch = self.cur_char.unwrap();
        match ch {
            '0' ... '9' | '.' => {
                // Number
                let mut s = String::new();
                s.push(ch);
                let mut has_dot = ch == '.';
                while let Some(ch) = self.next_char() {
                    match ch {
                        '0'...'9' => s.push(ch),
                        '.' if !has_dot => s.push(ch),
                        '.' => return self.make_error(ErrorKind::TooManyDotsInNumberLiteral),
                        _ => break,
                    }
                }
                if s == "." {
                    // If we only got one dot, it's an operator.
                    return self.make_result(TokenKind::Operator, s)
                }
                return self.make_result(TokenKind::Number, s);
            }

            'A' ... 'Z' => {
                // Identifier or keyword
                let mut s = String::new();
                s.push(ch);
                while let Some(ch) = self.next_char() {
                    match ch {
                        '0'...'9'|'A'...'Z' => s.push(ch),
                        _ => break,
                    }
                }
                return self.make_result(TokenKind::Identifier, s);
            }

            '(' | ')' | '=' | ';' | '+' | '-' | '*' | '/' | ':' => {
                // Single-character operator
                let mut s = String::new();
                s.push(ch);
                self.next_char();
                return self.make_result(TokenKind::Operator, s);
            }

            '<' | '>' => {
                // Potentially double-character operator
                let mut s = String::new();
                s.push(ch);
                if self.next_char() == Some('=') {
                    s.push('=');
                    self.next_char();
                }
                return self.make_result(TokenKind::Operator, s);
            }

            '"' => {
                // String literal
                let mut s = String::new();
                loop {
                    if let Some(ch) = self.next_char() {
                        match ch {
                            '"' => break,
                            '\n' => return self.make_error(ErrorKind::EolInStringLiteral),
                            _ => s.push(ch),
                        }
                    } else {
                        return self.make_error(ErrorKind::EofInStringLiteral);
                    }
                }
                self.next_char();
                return self.make_result(TokenKind::String, s);
            }

            _ => {
                // Error!
                return self.make_error(ErrorKind::UnexpectedCharacter)
            }
        }
    }
}

enum ErrorKind {
    Eof,
    EofInStringLiteral,
    EolInStringLiteral,
    TooManyDotsInNumberLiteral,
    UnexpectedCharacter,
}

struct Error {
    kind: ErrorKind,
    position: Position,
}
