use std::fmt;
use util::Position;

#[allow(dead_code)]
enum Keyword {
    DEF,
    END,
    FOR,
    IF,
    LET,
    NEXT,
    PRINT,
    STEP,
    THEN,
    TO,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    Identifier,
    NewLine,
    Number,
    Operator,
    String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    kind: TokenKind,
    repr: String,
    position: Position,
}

impl Token {
    pub fn new(kind: TokenKind, repr: String, position: Position) -> Token {
        Token {
            kind: kind,
            repr: repr,
            position: position,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?} at {} [{}]", self.kind, self.position, self.repr)
    }
}