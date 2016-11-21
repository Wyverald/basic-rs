use util::Position;

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

pub enum TokenKind {
    Identifier,
    Number,
    Operator,
    String,
}

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