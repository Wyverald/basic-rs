mod token;

pub use lexer::token::Token;

use std::io;
use std::str;
use util;

struct Position {
    pub line: u32,
    pub col: u32,
}

impl Position {
    pub fn new() -> Position {
        Position {
            line: 0,
            col: 0,
        }
    }

    pub fn next_col(&mut self) {
        self.col += 1;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 0;
    }
}

struct Lexer<'a> {
    input: str::Lines<'a>,
    cur_line: Option<str::Chars<'a>>,
    position: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lines = input.lines();
        let cur_line = lines.next();
        Lexer {
            input: lines,
            cur_line: cur_line.map(|s| s.chars()),
            position: Position::new(),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        loop {
            if self.cur_line.is_none() {
                return None;
            }

            let char: Option<char> = self.cur_line.as_mut().unwrap().next();
            if char.is_none() {
                self.cur_line = self.input.next().map(|s| s.chars());
                self.position.next_line();
                continue;
            }

            self.position.next_col();
            return char;
        }
    }

    pub fn next(&mut self) -> Result<Token, Error> {
        let ch = self.next_char().unwrap();
        let mut state = State::Initial;
        match state {
            State::Initial => {
                if ch.is_digit(10) {
                    state = State::NumberIntegral;
                } else {
                    state = State::NumberIntegral;
                }
            }
            State::NumberIntegral => {
                state = State::NumberIntegral;
            }
        }
        unimplemented!()
    }
}

enum Error {
    Io(io::Error),
}

enum State {
    Initial,
    NumberIntegral,
}
