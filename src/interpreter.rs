extern crate rand;

use ast::*;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::io::Write;
use std::mem;
use std::ops;

pub struct Interpreter {
    ast: Ast,
    environment: Environment,
}

struct Environment {
    pc: StatementIndex,
    column: usize,
    vars: HashMap<String, Value>,
    fns: HashMap<String, StatementIndex>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            pc: StatementIndex(0),
            column: 0,
            vars: HashMap::new(),
            fns: HashMap::new(),
        }
    }

    fn execute(&mut self, ast: &Ast, statement: &Statement) -> Result<ExecutionResult, Error> {
        match statement {
            &Statement::Def { name: ref identifier, parameters: _, body: _ } => {
                self.fns.insert(identifier.name.clone(), self.pc);
                Ok(ExecutionResult::NextStatement)
            },
            &Statement::Print(ref print_exprs) => {
                if print_exprs.is_empty() {
                    println!("");
                }
                for print_expr in print_exprs {
                    let value = self.evaluate(ast, &print_expr.expression)?;
                    let repr = value.to_string();
                    print!("{}", repr);
                    self.column += repr.len();
                    match print_expr.follow_type {
                        PrintFollowType::Immediate => (),
                        PrintFollowType::NextLine => {
                            println!("");
                            self.column = 0;
                        },
                        PrintFollowType::NextZone => {
                            print!(" ");
                            self.column += 1;
                        },
                    }
                }
                Ok(ExecutionResult::NextStatement)
            },
            &Statement::End => Ok(ExecutionResult::End),
            &Statement::Stop => Ok(ExecutionResult::Stop),
            &Statement::Let(ref identifier, ref expr) => {
                let value = self.evaluate(ast, expr)?;
                self.vars.insert(identifier.name.clone(), value);
                Ok(ExecutionResult::NextStatement)
            },
            &Statement::GoTo(line_number) => {
                Ok(ExecutionResult::GoTo(ast.line_numbers[&line_number]))
            },
            &Statement::If { ref condition, ref then_branch, ref else_branch } => {
                let condition_val = self.evaluate(ast, condition)?;
                if condition_val.into() {
                    self.execute(ast, then_branch.as_ref())
                } else {
                    match else_branch {
                        &Some(ref stmt) => self.execute(ast, stmt.as_ref()),
                        &None => Ok(ExecutionResult::NextStatement),
                    }
                }
            },
            &Statement::Input(ref variable) => {
                let val = {
                    let val: Value;
                    loop {
                        print!("?");
                        io::stdout().flush().expect("toilet stuck, could not flush");
                        let mut s = String::new();
                        io::stdin().read_line(&mut s).expect("failed to read from stdin");
                        let trimmed = s.trim();
                        match trimmed.parse::<i32>() {
                            Ok(i) => { val = Value::Integer(i); break }
                            Err(_) => match trimmed.parse::<f64>() {
                                Ok(f) => { val = Value::Float(f); break }
                                Err(_) => print!("?Redo from start"),
                            }
                        }
                    }
                    val
                };
                self.column = 0;
                self.vars.insert(variable.name.clone(), val);
                Ok(ExecutionResult::NextStatement)
            },
            &Statement::NoOp => Ok(ExecutionResult::NextStatement),
            &Statement::For { ref variable, ref from, to: _, step: _ } => {
                let from_val = self.evaluate(ast, from)?;
                self.vars.insert(variable.name.clone(), from_val);
                Ok(ExecutionResult::NextStatement)
            },
            &Statement::Next { for_index } => {
                if let Statement::For { ref variable, from: _, ref to, ref step } =
                        ast.statements[for_index.0] {
                    if self.vars.contains_key(&variable.name) {
                        let to_val = self.evaluate(ast, to)?;
                        let step_val = match step {
                            &Some(ref step_expr) => self.evaluate(ast, step_expr)?,
                            &None => Value::Integer(1),
                        };
                        let cur_val = self.vars.get_mut(&variable.name).unwrap();
                        let step_pos = step_val.clone().gt(Value::Integer(0)).into();
                        let step_neg = step_val.clone().lt(Value::Integer(0)).into();
                        *cur_val += step_val;
                        if step_neg && cur_val.clone().lt(to_val.clone()).into() ||
                                step_pos && cur_val.clone().gt(to_val.clone()).into() {
                            Ok(ExecutionResult::NextStatement)
                        } else {
                            Ok(ExecutionResult::GoTo(for_index.next()))
                        }
                    } else {
                        Err(Error("missing variable"))
                    }
                } else {
                    unreachable!()
                }
            },
        }
    }

    fn evaluate(&mut self, ast: &Ast, expr: &Expression) -> Result<Value, Error> {
        match expr {
            &Expression::Binary(bin_op, ref expr1, ref expr2) => {
                let val1 = self.evaluate(ast, expr1)?;
                let val2 = self.evaluate(ast, expr2)?;
                Ok(match bin_op {
                    BinaryOperator::Divide => val1 / val2,
                    BinaryOperator::Equals => val1.eq(val2),
                    BinaryOperator::GreaterThan => val1.gt(val2),
                    BinaryOperator::GreaterThanOrEqualTo => val1.ge(val2),
                    BinaryOperator::LessThan => val1.lt(val2),
                    BinaryOperator::LessThanOrEqualTo => val1.le(val2),
                    BinaryOperator::Minus => val1 - val2,
                    BinaryOperator::Multiply => val1 * val2,
                    BinaryOperator::Plus => val1 + val2,
                    BinaryOperator::Unequal => val1.ne(val2),
                })
            },
            &Expression::Unary(un_op, ref expr) => {
                let val = self.evaluate(ast, expr)?;
                Ok(match un_op {
                    UnaryOperator::Minus => Value::Integer(0) - val,
                })
            },
            &Expression::FloatLiteral(f) => Ok(Value::Float(f)),
            &Expression::IntegerLiteral(i) => Ok(Value::Integer(i)),
            &Expression::StringLiteral(ref s) => Ok(Value::String(s.clone())),
            &Expression::Identifier(ref identifier) => {
                if let Some(val) = self.vars.get(&identifier.name) {
                    Ok(val.clone())
                } else {
                    Err(Error("missing variable!"))
                }
            },
            &Expression::FnCall { ref name, ref arguments } => {
                match name.name.as_str() {
                    "EXP" => {
                        if arguments.len() != 1 {
                            Err(Error("too many arguments"))
                        } else {
                            let val = self.evaluate(ast, &arguments[0])?;
                            if let Some(f) = val.to_float() {
                                Ok(Value::Float(f.exp()))
                            } else {
                                Err(Error("EXP(float)"))
                            }
                        }
                    },
                    "INT" => {
                        if arguments.len() != 1 {
                            Err(Error("too many arguments"))
                        } else {
                            let val = self.evaluate(ast, &arguments[0])?;
                            if let Some(f) = val.to_float() {
                                Ok(Value::Integer(f.trunc() as i32))
                            } else {
                                Err(Error("INT(float)"))
                            }
                        }
                    },
                    "RND" => {
                        if arguments.len() != 1 {
                            Err(Error("too many arguments"))
                        } else {
                            Ok(Value::Float(rand::random()))
                        }
                    }
                    "SQR" => {
                        if arguments.len() != 1 {
                            Err(Error("too many arguments"))
                        } else {
                            let val = self.evaluate(ast, &arguments[0])?;
                            if let Some(f) = val.to_float() {
                                Ok(Value::Float(f.sqrt()))
                            } else {
                                Err(Error("SQR(float)"))
                            }
                        }
                    },
                    "TAB" => {
                        if arguments.len() != 1 {
                            Err(Error("too many arguments"))
                        } else {
                            let val = self.evaluate(ast, &arguments[0])?;
                            if let Some(i) = val.to_integer() {
                                let n = i as usize;
                                if self.column > n {
                                    println!("");
                                    self.column = 0;
                                }
                                print!("{:width$}", "", width = n - self.column);
                                self.column = n;
                                Ok(Value::String(String::new()))
                            } else {
                                Err(Error("TAB(int)"))
                            }
                        }
                    },
                    _ => {
                        let &def_stmt_index = match self.fns.get(&name.name) {
                            Some(present) => present,
                            None => return Err(Error("missing fn!")),
                        };
                        match ast.statements[def_stmt_index.0] {
                            Statement::Def { name: _, ref parameters, ref body } => {
                                if arguments.len() != parameters.len() {
                                    return Err(Error("mismatching number of arguments"));
                                }

                                let mut fn_vars: HashMap<String, Value> = HashMap::new();
                                for (param, arg) in parameters.iter().zip(arguments.iter()) {
                                    fn_vars.insert(param.name.clone(), self.evaluate(ast, arg)?);
                                }
                                mem::swap(&mut fn_vars, &mut self.vars);
                                let result = self.evaluate(ast, body)?;
                                mem::swap(&mut fn_vars, &mut self.vars);
                                Ok(result)
                            }
                            _ => unreachable!()
                        }
                    },
                }
            }
        }
    }
}

#[derive(Clone)]
enum Value {
    Integer(i32),
    Float(f64),
    String(String),
}

impl Value {
    fn from_bool(b: bool) -> Value {
        Value::Integer(if b { 1 } else { 0 })
    }

    fn to_integer(&self) -> Option<i32> {
        match self {
            &Value::Integer(i) => Some(i),
            _ => None,
        }
    }

    fn to_float(&self) -> Option<f64> {
        match self {
            &Value::Integer(i) => Some(i as f64),
            &Value::Float(f) => Some(f),
            _ => None,
        }
    }

    fn lt(self, other: Value) -> Value {
        if let (Some(l), Some(r)) = (self.to_float(), other.to_float()) {
            Value::from_bool(l < r)
        } else {
            Value::from_bool(self.to_string() < other.to_string())
        }
    }

    fn eq(self, other: Value) -> Value {
        if let (Some(l), Some(r)) = (self.to_float(), other.to_float()) {
            Value::from_bool(l == r)
        } else {
            Value::from_bool(self.to_string() == other.to_string())
        }
    }

    fn ne(self, other: Value) -> Value {
        if let (Some(l), Some(r)) = (self.to_float(), other.to_float()) {
            Value::from_bool(l != r)
        } else {
            Value::from_bool(self.to_string() != other.to_string())
        }
    }

    fn le(self, other: Value) -> Value {
        if let (Some(l), Some(r)) = (self.to_float(), other.to_float()) {
            Value::from_bool(l <= r)
        } else {
            Value::from_bool(self.to_string() <= other.to_string())
        }
    }

    fn gt(self, other: Value) -> Value {
        other.lt(self)
    }

    fn ge(self, other: Value) -> Value {
        other.le(self)
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::Integer(i) => i != 0,
            Value::Float(f) => f != 0.0,
            Value::String(_) => true,
        }
    }
}

impl ops::Add for Value {
    type Output = Value;
    fn add(self, other: Value) -> Value {
        if let (Some(l), Some(r)) = (self.to_integer(), other.to_integer()) {
            Value::Integer(l + r)
        } else if let (Some(l), Some(r)) = (self.to_float(), other.to_float()) {
            Value::Float(l + r)
        } else {
            Value::String(self.to_string() + &other.to_string())
        }
    }
}

impl ops::Sub for Value {
    type Output = Value;
    fn sub(self, other: Value) -> Value {
        if let (Some(l), Some(r)) = (self.to_integer(), other.to_integer()) {
            Value::Integer(l - r)
        } else if let (Some(l), Some(r)) = (self.to_float(), other.to_float()) {
            Value::Float(l - r)
        } else {
            panic!("wut")
        }
    }
}

impl ops::Mul for Value {
    type Output = Value;
    fn mul(self, other: Value) -> Value {
        if let (Some(l), Some(r)) = (self.to_integer(), other.to_integer()) {
            Value::Integer(l * r)
        } else if let (Some(l), Some(r)) = (self.to_float(), other.to_float()) {
            Value::Float(l * r)
        } else {
            panic!("wut")
        }
    }
}

impl ops::Div for Value {
    type Output = Value;
    fn div(self, other: Value) -> Value {
        if let (Some(l), Some(r)) = (self.to_integer(), other.to_integer()) {
            Value::Integer(l / r)
        } else if let (Some(l), Some(r)) = (self.to_float(), other.to_float()) {
            Value::Float(l / r)
        } else {
            panic!("wut")
        }
    }
}

impl ops::AddAssign for Value {
    fn add_assign(&mut self, other: Value) {
        if let &mut Value::String(ref mut s) = self {
            s.push_str(&other.to_string());
        } else {
            *self = self.clone() + other;
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Value::Integer(i) => write!(f, " {} ", i),
            &Value::Float(fl) => write!(f, " {} ", fl),
            &Value::String(ref s) => write!(f, "{}", s),
        }
    }
}

enum ExecutionResult {
    End,
    Stop,
    NextStatement,
    GoTo(StatementIndex),
}

impl Interpreter {
    pub fn new(ast: Ast) -> Interpreter {
        Interpreter {
            ast: ast,
            environment: Environment::new(),
        }
    }

    pub fn reset(&mut self) {
        self.environment = Environment::new();
    }

    pub fn run(&mut self) -> Result<(), Error> {
        while self.step()? {}
        Ok(())
    }

    // Returns "should continue"
    pub fn step(&mut self) -> Result<bool, Error> {
        let ref statement = self.ast.statements[self.environment.pc.0];
        let result = self.environment.execute(&self.ast, statement)?;
        //println!("Executing {}", self.environment.pc.0);
        self.environment.pc = match result {
            ExecutionResult::NextStatement => self.environment.pc.next(),
            ExecutionResult::GoTo(stmt_idx) => stmt_idx,
            ExecutionResult::End => return Ok(false),
            ExecutionResult::Stop => return Ok(false),
        };
        Ok(true)
    }
}

#[derive(Debug)]
pub struct Error(&'static str);
