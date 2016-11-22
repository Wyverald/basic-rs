use ast::*;
use lexer::Keyword;
use lexer::Token;
use lexer::TokenContent;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use util::Position;

pub struct Parser<I: Iterator<Item=Token>> {
    input: Peekable<I>,
    ast: Ast,
}

impl<I: Iterator<Item=Token>> Parser<I> {
    pub fn new<II: IntoIterator<Item=Token, IntoIter=I>>(input: II) -> Parser<I> {
        Parser {
            input: input.into_iter().peekable(),
            ast: Ast {
                statements: vec!(),
                line_numbers: HashMap::new(),
            },
        }
    }

    pub fn run(&mut self) -> Result<&Ast, Error> {
        while let Some(_) = self.input.peek() {
            self.parse_line()?
        }
        Ok(&self.ast)
    }

    fn next_token(&mut self) -> Result<Token, Error> {
        match self.input.next() {
            None => Err(Error::unexpected_eof()),
            Some(token) => Ok(token),
        }
    }

    fn peek_token(&mut self) -> Result<&Token, Error> {
        match self.input.peek() {
            None => Err(Error::unexpected_eof()),
            Some(token) => Ok(token),
        }
    }

    fn expect_identifier(&mut self) -> Result<Identifier, Error> {
        let token = self.next_token()?;
        match token.content {
            TokenContent::Identifier(s) => Ok(Identifier { name: s, position: token.position }),
            _ => Err(Error::unexpected_token(token, "expected identifier")),
        }
    }

    fn expect_line_number(&mut self) -> Result<LineNumber, Error> {
        let token = self.next_token()?;
        match token.content {
            TokenContent::IntegerLiteral(num) => Ok(LineNumber(num)),
            _ => Err(Error::unexpected_token(token, "expected a line number")),
        }
    }

    fn expect_simple_token(&mut self, expected_content: TokenContent) -> Result<(), Error> {
        let token = self.next_token()?;
        if token.content == expected_content {
            Ok(())
        } else {
            Err(Error::unexpected_token(token, format!("expected {:?}", expected_content)))
        }
    }

    fn parse_line(&mut self) -> Result<(), Error> {
        let cur_position = self.peek_token()?.position;
        let line_number = self.expect_line_number()?;
        let statement_index = StatementIndex(self.ast.statements.len());
        if self.ast.line_numbers.contains_key(&line_number) {
            return Err(Error { kind: ErrorKind::DuplicateLineNumber, position: cur_position })
        }
        self.ast.line_numbers.insert(line_number, statement_index);

        loop {
            let statement = self.parse_statement()?;
            self.ast.statements.push(statement);
            let token = self.next_token()?;
            match token.content {
                TokenContent::NewLine => break,
                TokenContent::Colon => continue,
                _ => return Err(Error::unexpected_token(token, "expected NewLine or Colon")),
            }
        }
        Ok(())
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        let token = self.next_token()?;
        match token.content {
            TokenContent::Keyword(Keyword::End) => Ok(Statement::End),
            TokenContent::Keyword(Keyword::GoTo) => Ok(Statement::GoTo(self.expect_line_number()?)),
            TokenContent::Keyword(Keyword::Let) => {
                let identifier = self.expect_identifier()?;
                self.parse_assignment(identifier)
            },
            TokenContent::Identifier(s) => {
                self.parse_assignment(Identifier { name: s, position: token.position })
            },
            TokenContent::Keyword(Keyword::Print) => {
                let mut print_exprs: Vec<PrintExpr> = vec!();
                loop {
                    match self.peek_token()?.content {
                        TokenContent::NewLine | TokenContent::Colon |
                        TokenContent::Keyword(Keyword::Else) => break,
                        _ => (),
                    };
                    let expr = self.parse_expression()?;
                    let (follow_type, skip_token) = match self.peek_token()?.content {
                        TokenContent::Semicolon => (PrintFollowType::Immediate, true),
                        TokenContent::Comma => (PrintFollowType::NextZone, true),
                        TokenContent::NewLine | TokenContent::Colon |
                        TokenContent::Keyword(Keyword::Else) => (PrintFollowType::NextLine, false),
                        _ => (PrintFollowType::Immediate, false),
                    };
                    print_exprs.push(PrintExpr {
                        expression: expr,
                        follow_type: follow_type,
                    });
                    if skip_token { self.next_token()?; }
                }
                Ok(Statement::Print(print_exprs))
            },
            TokenContent::Keyword(Keyword::Def) => {
                let identifier = self.expect_identifier()?;
                self.expect_simple_token(TokenContent::LeftParens)?;
                let mut arguments: Vec<Identifier> = vec!();
                loop {
                    arguments.push(self.expect_identifier()?);
                    let token = self.next_token()?;
                    match token.content {
                        TokenContent::Comma => continue,
                        TokenContent::RightParens => break,
                        _ => return {
                            Err(Error::unexpected_token(token, "expected RightParens or Comma"))
                        },
                    }
                }
                self.expect_simple_token(TokenContent::Equals)?;
                let expr = self.parse_expression()?;
                Ok(Statement::Definition {
                    name: identifier,
                    arguments: arguments,
                    body: expr,
                })
            },
            TokenContent::Keyword(Keyword::If) => {
                let condition = self.parse_expression()?;
                let token = self.next_token()?;
                let then_branch = match token.content {
                    TokenContent::Keyword(Keyword::GoTo) => {
                        Statement::GoTo(self.expect_line_number()?)
                    },
                    TokenContent::Keyword(Keyword::Then) => {
                        let line_number_follows = match self.peek_token()?.content {
                            TokenContent::IntegerLiteral(_) => true,
                            _ => false,
                        };
                        if line_number_follows {
                            Statement::GoTo(self.expect_line_number()?)
                        } else {
                            self.parse_statement()?
                        }
                    },
                    _ => return Err(Error::unexpected_token(token, "expected THEN or GOTO")),
                };
                let else_follows = match self.peek_token()?.content {
                    TokenContent::Keyword(Keyword::Else) => true,
                    _ => false,
                };
                let else_branch = if else_follows {
                    self.next_token()?;
                    Some(self.parse_statement()?)
                } else {
                    None
                };
                Ok(Statement::If {
                    condition: condition,
                    then_branch: Box::new(then_branch),
                    else_branch: else_branch.map(Box::new),
                })
            },
            TokenContent::Keyword(Keyword::For) => {
                let variable = self.expect_identifier()?;
                self.expect_simple_token(TokenContent::Equals)?;
                let from = self.parse_expression()?;
                self.expect_simple_token(TokenContent::Keyword(Keyword::To))?;
                let to = self.parse_expression()?;
                let step_follows = match self.peek_token()?.content {
                    TokenContent::Keyword(Keyword::Step) => true,
                    _ => false,
                };
                let step = if step_follows {
                    self.next_token()?;
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                Ok(Statement::For {
                    variable: variable,
                    from: from,
                    to: to,
                    step: step,
                })
            },
            TokenContent::Keyword(Keyword::Next) => {
                // TODO: This logic is wrong. It naively looks for the last FOR without accounting
                // for nested loops.
                let var_follows = match self.peek_token()?.content {
                    TokenContent::Identifier(_) => true,
                    _ => false,
                };
                if var_follows { self.expect_identifier()?; }

                let statements = &self.ast.statements;
                let for_index = statements.into_iter().rposition(|ref statement|
                    match **statement {
                        Statement::For {
                            variable: _,
                            from: _,
                            to: _,
                            step: _,
                        } => true,
                        _ => false,
                    });
                match for_index {
                    Some(index) => Ok(Statement::Next { for_index: StatementIndex(index) }),
                    None => {
                        return Err(
                            Error { kind: ErrorKind::UnmatchedNext, position: token.position })
                    }
                }
            }
            _ => return Err(Error::unexpected_token(token, "expected statement")),
        }
    }

    fn parse_assignment(&mut self, ident: Identifier) -> Result<Statement, Error> {
        self.expect_simple_token(TokenContent::Equals)?;
        let expr = self.parse_expression()?;
        Ok(Statement::Let(ident, expr))
    }

    // Expression = Choice (("<"|">"|"="|"<="|">=") Choice)?
    fn parse_expression(&mut self) -> Result<Expression, Error> {
        let choice = self.parse_choice()?;
        let opt_operator = match self.peek_token()?.content {
            TokenContent::LessThan => Some(BinaryOperator::LessThan),
            TokenContent::GreaterThan => Some(BinaryOperator::GreaterThan),
            TokenContent::Equals => Some(BinaryOperator::Equals),
            TokenContent::LeftDoubleArrow => Some(BinaryOperator::LessThanOrEqualTo),
            TokenContent::RightDoubleArrow => Some(BinaryOperator::GreaterThanOrEqualTo),
            _ => None,
        };
        match opt_operator {
            Some(operator) => {
                self.next_token()?;
                Ok(Expression::Binary(operator, Box::new(choice), Box::new(self.parse_choice()?)))
            },
            None => Ok(choice),
        }
    }

    // Choice = "-" Choice | Term (("+"|"-") Term)*
    fn parse_choice(&mut self) -> Result<Expression, Error> {
        let is_minus = match self.peek_token()?.content {
            TokenContent::Minus => true,
            _ => false,
        };
        if is_minus {
            self.next_token()?;
            Ok(Expression::Unary(UnaryOperator::Minus, Box::new(self.parse_choice()?)))
        } else {
            let mut expr = self.parse_term()?;
            loop {
                let opt_operator = match self.peek_token()?.content {
                    TokenContent::Plus => Some(BinaryOperator::Plus),
                    TokenContent::Minus => Some(BinaryOperator::Minus),
                    _ => None,
                };
                match opt_operator {
                    Some(operator) => {
                        self.next_token()?;
                        expr = Expression::Binary(
                            operator, Box::new(expr), Box::new(self.parse_term()?))
                    },
                    None => break,
                }
            }
            Ok(expr)
        }
    }

    // Term = Factor (("*"|"/") Factor)*
    fn parse_term(&mut self) -> Result<Expression, Error> {
        let mut expr = self.parse_factor()?;
        loop {
            let opt_operator = match self.peek_token()?.content {
                TokenContent::Asterisk => Some(BinaryOperator::Multiply),
                TokenContent::Slash => Some(BinaryOperator::Divide),
                _ => None,
            };
            match opt_operator {
                Some(operator) => {
                    self.next_token()?;
                    expr = Expression::Binary(
                        operator, Box::new(expr), Box::new(self.parse_factor()?))
                },
                None => break,
            }
        }
        Ok(expr)
    }

    // Factor = StringLiteral | IntegerLiteral | FloatLiteral | Identifier |
    //          "(" Expression ")" | Identifier "(" Expression ("," Expression)* ")"
    fn parse_factor(&mut self) -> Result<Expression, Error> {
        let token = self.next_token()?;
        let expr = match token.content {
            TokenContent::StringLiteral(s) => Expression::StringLiteral(s),
            TokenContent::IntegerLiteral(i) => Expression::IntegerLiteral(i),
            TokenContent::FloatLiteral(f) => Expression::FloatLiteral(f),
            TokenContent::LeftParens => {
                let expr = self.parse_expression()?;
                self.expect_simple_token(TokenContent::RightParens)?;
                expr
            },
            TokenContent::Identifier(s) => {
                let identifier = Identifier { name: s, position: token.position };
                let is_function_call = match self.peek_token()?.content {
                    TokenContent::LeftParens => true,
                    _ => false,
                };
                if is_function_call {
                    self.next_token()?;
                    let mut arguments: Vec<Expression> = vec!();
                    arguments.push(self.parse_expression()?);
                    loop {
                        match self.peek_token()?.content {
                            TokenContent::Comma => (),
                            _ => break,
                        };
                        self.next_token()?;
                        arguments.push(self.parse_expression()?);
                    }
                    self.expect_simple_token(TokenContent::RightParens)?;
                    Expression::FnCall {
                        name: identifier,
                        arguments: arguments,
                    }
                } else {
                    Expression::Identifier(identifier)
                }
            },
            _ => return Err(Error::unexpected_token(token, "expected expression")),
        };
        Ok(expr)
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    DuplicateLineNumber,
    UnexpectedToken(TokenContent, String),
    UnexpectedEndOfFile,
    UnmatchedNext,
}

pub struct Error {
    kind: ErrorKind,
    position: Position,
}

impl Error {
    pub fn unexpected_eof() -> Error {
        Error { kind: ErrorKind::UnexpectedEndOfFile, position: Position::new() }
    }

    pub fn unexpected_token<S: Into<String>>(token: Token, message: S) -> Error {
        Error {
            kind: ErrorKind::UnexpectedToken(token.content, message.into()),
            position: token.position
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Parser error at {}: {:?}", self.position, self.kind)
    }
}
