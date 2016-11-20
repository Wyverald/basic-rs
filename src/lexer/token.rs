pub enum Keyword {
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

pub enum Token {
    NUMBER(String),
    STRING(String),
    KEYWORD(Keyword),
    IDENT(String),
}
