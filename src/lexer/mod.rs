mod token;

pub use lexer::token::Keyword;
pub use lexer::token::Token;
pub use lexer::token::TokenContent;

use std::fmt;
use std::str;
use util::Position;

pub struct Lexer<'a> {
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
        self.cur_char = self.next_char_internal();
        self.cur_char
    }

    fn next_char_internal(&mut self) -> Option<char> {
        self.cur_position = self.next_position;

        if self.cur_line.is_none() {
            return None;
        }

        let char: Option<char> = self.cur_line.as_mut().unwrap().next();
        if char.is_none() {
            self.cur_line = self.input.next().map(|s| s.chars());
            self.next_position.next_line();
            return Some('\n');
        }

        self.next_position.next_col();
        return char;
    }

    fn make_error(&self, error_kind: ErrorKind) -> Result<Token, Error> {
        Err(Error { kind: error_kind, position: self.cur_position })
    }

    // Should only be called when there is a next character.
    fn next_token(&mut self) -> Result<Token, Error> {
        let ch = self.cur_char.unwrap();
        let cur_position = self.cur_position;
        match ch {
            '0' ... '9' | '.' => {
                // Number
                let mut s = String::new();
                s.push(ch);
                let mut has_dot = ch == '.';
                while let Some(ch) = self.next_char() {
                    match ch {
                        '0'...'9' => s.push(ch),
                        '.' if !has_dot => {
                            has_dot = true;
                            s.push(ch);
                        },
                        '.' => return self.make_error(ErrorKind::TooManyDotsInNumberLiteral),
                        _ => break,
                    }
                }
                if s == "." {
                    // If we only got one dot, it's an operator.
                    return Ok(Token::new(TokenContent::Dot, cur_position));
                }
                let content = if has_dot {
                    TokenContent::FloatLiteral(s.parse().unwrap())
                } else {
                    TokenContent::IntegerLiteral(s.parse().unwrap())
                };
                return Ok(Token::new(content, cur_position));
            }

            'A' ... 'Z' | '$' => {
                // Identifier or keyword
                let mut s = String::new();
                s.push(ch);
                while let Some(ch) = self.next_char() {
                    match ch {
                        '0'...'9'|'A'...'Z'|'$' => s.push(ch),
                        _ => break,
                    }
                }
                return Ok(Token::new(TokenContent::keyword_or_identifier(s), cur_position));
            }

            '(' | ')' | '=' | ';' | '+' | '-' | '*' | '/' | ':' | ',' | '\n' => {
                // Single-character symbol
                let content = match ch {
                    '(' => TokenContent::LeftParens,
                    ')' => TokenContent::RightParens,
                    '=' => TokenContent::Equals,
                    ';' => TokenContent::Semicolon,
                    '+' => TokenContent::Plus,
                    '-' => TokenContent::Minus,
                    '*' => TokenContent::Asterisk,
                    '/' => TokenContent::Slash,
                    ':' => TokenContent::Colon,
                    ',' => TokenContent::Comma,
                    '\n' => TokenContent::NewLine,
                    _ => unreachable!(),
                };
                self.next_char();
                return Ok(Token::new(content, cur_position));
            }

            '<' | '>' => {
                // Potentially double-character symbol
                let content = if self.next_char() == Some('=') {
                    self.next_char();
                    if ch == '<' { TokenContent::LeftDoubleArrow }
                    else { TokenContent::RightDoubleArrow }
                } else {
                    if ch == '<' { TokenContent::LessThan }
                    else { TokenContent::GreaterThan }
                };
                return Ok(Token::new(content, cur_position));
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
                return Ok(Token::new(TokenContent::StringLiteral(s), cur_position));
            }

            _ => {
                // Error!
                return self.make_error(ErrorKind::UnexpectedCharacter)
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ch) = self.cur_char {
                if ch != ' ' { break }
                self.next_char();
            } else {
                return None;
            }
        }

        Some(self.next_token())
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    EofInStringLiteral,
    EolInStringLiteral,
    TooManyDotsInNumberLiteral,
    UnexpectedCharacter,
}

#[derive(Copy, Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub position: Position,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Lexer error at {}: {:?}", self.position, self.kind)
    }
}
