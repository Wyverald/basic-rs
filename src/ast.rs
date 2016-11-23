use std::collections::HashMap;
use util::Position;

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LineNumber(pub u32);

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct StatementIndex(pub usize);

impl StatementIndex {
    pub fn next(self) -> StatementIndex {
        StatementIndex(self.0 + 1)
    }
}

#[derive(Debug)]
pub struct Ast {
    pub statements: Vec<Statement>,
    pub line_numbers: HashMap<LineNumber, StatementIndex>,
}

#[derive(Debug)]
pub enum Statement {
    Def {
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Expression,
    },
    End,
    For {
        variable: Identifier,
        from: Expression,
        to: Expression,
        step: Option<Expression>,
    },
    GoTo(LineNumber),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Input(Identifier),
    Let(Identifier, Expression),
    Next {
        for_index: StatementIndex,
    },
    NoOp,
    Print(Vec<PrintExpr>),
    Stop,
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
    pub position: Position,
}

#[derive(Debug)]
pub enum Expression {
    StringLiteral(String),
    IntegerLiteral(i32),
    FloatLiteral(f64),
    Identifier(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    FnCall {
        name: Identifier,
        arguments: Vec<Expression>,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOperator {
    Minus,
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryOperator {
    Divide,
    Equals,
    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,
    Plus,
    Minus,
    Multiply,
    Unequal,
}

#[derive(Debug)]
pub struct PrintExpr {
    pub expression: Expression,
    pub follow_type: PrintFollowType,
}

#[derive(Debug)]
pub enum PrintFollowType {
    Immediate,
    NextLine,
    NextZone,
}
