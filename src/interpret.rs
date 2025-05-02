use crate::parser;
use crate::parser::Expr;
use crate::parser::Node;
use crate::token::Lexer;
use crate::token::{KeywordKind, LiteralKind, Token};
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

type InterpreterResult = Result<InterpreterValue, miette::Error>;

macro_rules! runtime_error {
    ($msg:literal, $line:literal) => {
        miette::miette!(code = "70", "{}", format!("{}\n[line {}]", $msg, $line))
    };
    ($msg:expr, $line:literal) => {
        miette::miette!(code = "70", "{}", format!("{}\n[line {}]", $msg, $line))
    };
}

#[derive(Debug)]
pub enum InterpreterValue {
    Bool(bool),
    Float(f64),
    String(String),
    Nil,
}

impl Display for InterpreterValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterValue::Bool(v) => write!(f, "{v}"),
            InterpreterValue::Float(v) => write!(f, "{v}"),
            InterpreterValue::String(v) => write!(f, "{v}"),
            InterpreterValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Default)]
pub struct Interpreter<'i> {
    src: Option<String>,
    file: Option<&'i PathBuf>,
}

impl<'i> Interpreter<'i> {
    pub fn new(file: &'i PathBuf) -> Self {
        Self {
            src: None,
            file: Some(file),
        }
    }

    fn filename(&self) -> String {
        self.file
            .as_ref()
            .map(|f| f.display().to_string())
            .unwrap_or_else(|| "stdin".to_string())
    }

    fn source(&self) -> &str {
        self.src.as_ref().map(|s| s.as_ref()).unwrap()
    }

    pub fn interpret(&mut self, source: String) -> Result<(), miette::Error> {
        self.src = Some(source);
        let result = self.evaluate()?;
        println!("{result}");
        Ok(())
    }

    fn evaluate(&self) -> InterpreterResult {
        let lexer = Lexer::new(self.source());
        let mut parser = parser::Parser::new(lexer.peekable());
        let ast = parser.ast()?;

        self.evaluate_all(&ast.tree)
    }

    pub fn evaluate_all(&self, node: &Node) -> InterpreterResult {
        match node {
            Node::Terminal(t) => Interpreter::eval_literal(t),
            Node::Expr(exp) => match exp.borrow() {
                Expr::Binary(l, op, r) => self.eval_binary_exp(l, op, r),
                Expr::Unary(op, exp) => self.eval_unary_exp(op, exp),
                Expr::Group(group_exp) => self.evaluate_all(group_exp),
            },
        }
    }

    fn eval_literal(token: &Token) -> InterpreterResult {
        match token {
            Token::Number { value, .. } => Ok(InterpreterValue::Float(*value)),
            Token::String { value } => Ok(InterpreterValue::String(value.clone())),
            Token::Keyword(KeywordKind::True) => Ok(InterpreterValue::Bool(true)),
            Token::Keyword(KeywordKind::False) => Ok(InterpreterValue::Bool(false)),
            Token::Keyword(KeywordKind::Nil) => Ok(InterpreterValue::Nil),
            _ => unimplemented!("{:?}", token),
        }
    }

    fn eval_binary_exp(&self, left: &Node, op: &Node, right: &Node) -> InterpreterResult {
        let left = self.evaluate_all(left)?;
        let right = self.evaluate_all(right)?;
        match op {
            Node::Terminal(token) => match token {
                Token::Literal(literal) => match literal {
                    LiteralKind::Plus => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Float(f_l + f_r)),
                            _ => Err(runtime_error!("Operands must be numbers", 1))?,
                        },
                        InterpreterValue::String(ref s_l) => match right {
                            InterpreterValue::String(s_r) => {
                                let mut s = String::from(s_l);
                                s.push_str(s_r.as_str());
                                Ok(InterpreterValue::String(s))
                            }
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    LiteralKind::Minus => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Float(f_l - f_r)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    LiteralKind::Star => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Float(f_l * f_r)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    LiteralKind::Slash => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Float(f_l / f_r)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    LiteralKind::EqEq => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Bool(f_l == f_r)),
                            _ => Ok(InterpreterValue::Bool(false)),
                        },
                        InterpreterValue::String(ref s_l) => match right {
                            InterpreterValue::String(ref s_r) => {
                                Ok(InterpreterValue::Bool(s_l == s_r))
                            }
                            _ => Ok(InterpreterValue::Bool(false)),
                        },
                        InterpreterValue::Bool(s_l) => match right {
                            InterpreterValue::Bool(s_r) => Ok(InterpreterValue::Bool(s_l == s_r)),
                            _ => Ok(InterpreterValue::Bool(false)),
                        },
                        InterpreterValue::Nil => match right {
                            InterpreterValue::Nil => Ok(InterpreterValue::Bool(true)),
                            _ => Ok(InterpreterValue::Bool(false)),
                        },
                    },
                    LiteralKind::BangEq => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Bool(f_l != f_r)),
                            _ => Ok(InterpreterValue::Bool(false)),
                        },
                        InterpreterValue::String(ref s_l) => match right {
                            InterpreterValue::String(ref s_r) => {
                                Ok(InterpreterValue::Bool(s_l != s_r))
                            }
                            _ => Ok(InterpreterValue::Bool(false)),
                        },
                        InterpreterValue::Bool(s_l) => match right {
                            InterpreterValue::Bool(s_r) => Ok(InterpreterValue::Bool(s_l != s_r)),
                            _ => Ok(InterpreterValue::Bool(false)),
                        },
                        InterpreterValue::Nil => match right {
                            InterpreterValue::Nil => Ok(InterpreterValue::Bool(false)),
                            _ => Ok(InterpreterValue::Bool(true)),
                        },
                    },
                    LiteralKind::Less => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Bool(f_l < f_r)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    LiteralKind::LessEq => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Bool(f_l <= f_r)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    LiteralKind::Greater => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Bool(f_l > f_r)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    LiteralKind::GreaterEq => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Bool(f_l >= f_r)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    _ => todo!("{literal}"),
                },
                Token::Keyword(keyword) => match keyword {
                    KeywordKind::And => match left {
                        InterpreterValue::Bool(l_b) => match right {
                            InterpreterValue::Bool(r_b) => Ok(InterpreterValue::Bool(l_b && r_b)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    KeywordKind::Or => match left {
                        InterpreterValue::Bool(l_b) => match right {
                            InterpreterValue::Bool(r_b) => Ok(InterpreterValue::Bool(l_b || r_b)),
                            _ => Err(runtime_error!(self.source(), 1))?,
                        },
                        _ => Err(runtime_error!(self.source(), 1))?,
                    },
                    _ => todo!("{keyword}"),
                },
                _ => todo!("{token}"),
            },
            _ => todo!("{op}"),
        }
    }

    fn eval_unary_exp(&self, op: &Node, exp: &Node) -> Result<InterpreterValue, miette::Error> {
        let val = self.evaluate_all(exp)?;
        match op {
            Node::Terminal(t) => match val {
                InterpreterValue::Bool(v) => {
                    if t.borrow() == &Token::Literal(LiteralKind::Bang) {
                        Ok(InterpreterValue::Bool(!v))
                    } else {
                        Ok(InterpreterValue::Bool(false))
                    }
                }
                InterpreterValue::Float(v) => {
                    if t.borrow() == &Token::Literal(LiteralKind::Minus) {
                        Ok(InterpreterValue::Float(-v))
                    } else if t.borrow() == &Token::Literal(LiteralKind::Bang) {
                        Ok(InterpreterValue::Bool(false))
                    } else {
                        Err(runtime_error!("Operand must be a number.", 1))?
                    }
                }
                InterpreterValue::Nil => {
                    if t.borrow() == &Token::Literal(LiteralKind::Bang) {
                        Ok(InterpreterValue::Bool(true))
                    } else {
                        Err(runtime_error!("Operand must be a number.", 1))?
                    }
                }
                _ => Err(runtime_error!("Operand must be a number.", 1))?,
            },
            _ => Err(runtime_error!("invalid operation {op} for {val}", 1))?,
        }
    }
}
