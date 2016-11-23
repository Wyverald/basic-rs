use std::fmt;
use util::Position;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    Def,
    Else,
    End,
    For,
    GoTo,
    If,
    Input,
    Let,
    Next,
    Print,
    Step,
    Stop,
    Then,
    To,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenContent {
    Asterisk,
    Colon,
    Comma,
    Dot,
    Equals,
    FloatLiteral(f64),
    GreaterThan,
    Keyword(Keyword),
    Identifier(String),
    IntegerLiteral(i32),
    LeftParens,
    LessThan,
    Minus,
    NewLine,
    Plus,
    Rem,
    RightParens,
    Semicolon,
    Slash,
    StringLiteral(String),
}

impl TokenContent {
    pub fn keyword_or_identifier(repr: String) -> TokenContent {
        let keyword = match repr.as_str() {
            "DEF" => Keyword::Def,
            "ELSE" => Keyword::Else,
            "END" => Keyword::End,
            "FOR" => Keyword::For,
            "GOTO" => Keyword::GoTo,
            "IF" => Keyword::If,
            "INPUT" => Keyword::Input,
            "LET" => Keyword::Let,
            "NEXT" => Keyword::Next,
            "PRINT" => Keyword::Print,
            "STEP" => Keyword::Step,
            "STOP" => Keyword::Stop,
            "THEN" => Keyword::Then,
            "TO" => Keyword::To,
            _ => return TokenContent::Identifier(repr),
        };
        TokenContent::Keyword(keyword)
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub content: TokenContent,
    pub position: Position,
}

impl Token {
    pub fn new(content: TokenContent, position: Position) -> Token {
        Token {
            content: content,
            position: position,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Token at {}: {:?}", self.position, self.content)
    }
}