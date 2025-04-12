use crate::parser::Ast;
use crate::parser::Expr;
use crate::parser::Node;
use crate::token::{KeywordKind, LiteralKind, Token};
use miette::miette;
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};

type InterpreterResult = Result<InterpreterValue, miette::Error>;

pub struct Interpreter {
    ast: Ast,
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

impl Interpreter {
    pub fn new(ast: Ast) -> Self {
        Self { ast }
    }

    pub fn evaluate(&self) -> InterpreterResult {
        self.evaluate_all(&self.ast.tree)
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
                            _ => Err(miette::miette!(
                                "type mismatch - can't add {left} to {right}"
                            )),
                        },
                        InterpreterValue::String(ref s_l) => match right {
                            InterpreterValue::String(s_r) => {
                                let mut s = String::from(s_l);
                                s.push_str(s_r.as_str());
                                Ok(InterpreterValue::String(s))
                            }
                            _ => Err(miette::miette!(
                                "type mismatch - can't add {left} to {right}"
                            )),
                        },
                        _ => Err(miette!("invalid operation {op} for {left} and {right}"))?,
                    },
                    LiteralKind::Minus => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Float(f_l - f_r)),
                            _ => Err(miette::miette!(
                                "type mismatch - can't subtract {right} from {left}"
                            )),
                        },
                        _ => Err(miette!("invalid operation {op} for {left} and {right}"))?,
                    },
                    LiteralKind::Star => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Float(f_l * f_r)),
                            _ => Err(miette::miette!(
                                "type mismatch - can't multiply {left} and {right}"
                            )),
                        },
                        _ => Err(miette!("invalid operation {op} for {left} and {right}"))?,
                    },
                    LiteralKind::Slash => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Float(f_l / f_r)),
                            _ => Err(miette::miette!(
                                "type mismatch - can't divide {left} and {right}"
                            )),
                        },
                        _ => Err(miette!("invalid operation {op} for {left} and {right}"))?,
                    },
                    LiteralKind::EqEq => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Bool(f_l == f_r)),
                            _ => Err(miette::miette!(
                                "type mismatch - can't divide {left} and {right}"
                            )),
                        },
                        InterpreterValue::String(ref s_l) => match right {
                            InterpreterValue::String(ref s_r) => {
                                Ok(InterpreterValue::Bool(s_l == s_r))
                            }
                            _ => Err(miette::miette!(
                                "type mismatch - can't compare {left} to {right}"
                            )),
                        },
                        InterpreterValue::Bool(s_l) => match right {
                            InterpreterValue::Bool(s_r) => Ok(InterpreterValue::Bool(s_l == s_r)),
                            _ => Err(miette::miette!(
                                "type mismatch - can't compare {left} to {right}"
                            )),
                        },
                        InterpreterValue::Nil => todo!("comparison with nil"),
                    },
                    LiteralKind::BangEq => match left {
                        InterpreterValue::Float(f_l) => match right {
                            InterpreterValue::Float(f_r) => Ok(InterpreterValue::Bool(f_l != f_r)),
                            _ => Err(miette::miette!(
                                "type mismatch - can't divide {left} and {right}"
                            )),
                        },
                        InterpreterValue::String(ref s_l) => match right {
                            InterpreterValue::String(ref s_r) => {
                                Ok(InterpreterValue::Bool(s_l != s_r))
                            }
                            _ => Err(miette::miette!(
                                "type mismatch - can't compare {left} to {right}"
                            )),
                        },
                        InterpreterValue::Bool(s_l) => match right {
                            InterpreterValue::Bool(s_r) => Ok(InterpreterValue::Bool(s_l != s_r)),
                            _ => Err(miette::miette!(
                                "type mismatch - can't compare {left} to {right}"
                            )),
                        },
                        InterpreterValue::Nil => todo!("comparison with nil"),
                    },
                    // LiteralKind::Less => {}
                    // LiteralKind::LessEq => {}
                    // LiteralKind::Greater => {}
                    // LiteralKind::GreaterEq => {}
                    _ => todo!("{literal}"),
                },
                Token::Keyword(keyword) => match keyword {
                    KeywordKind::And => match left {
                        InterpreterValue::Bool(l_b) => match right {
                            InterpreterValue::Bool(r_b) => Ok(InterpreterValue::Bool(l_b && r_b)),
                            _ => {
                                // todo: to take advantage of miette would be nice to have row/col info here
                                Err(miette::miette!(
                                    "type mismatch - can't 'and' {left} and {right}"
                                ))
                            }
                        },
                        _ => Err(miette!("invalid operation {op} for {left}"))?,
                    },
                    KeywordKind::Or => match left {
                        InterpreterValue::Bool(l_b) => match right {
                            InterpreterValue::Bool(r_b) => Ok(InterpreterValue::Bool(l_b || r_b)),
                            _ => Err(miette::miette!(
                                "type mismatch - can't 'or' {left} and {right}"
                            )),
                        },
                        _ => Err(miette!("invalid operation {op} for {left} and {right}"))?,
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
                        Err(miette!("invalid operation {op} for {val}"))?
                    }
                }
                InterpreterValue::Float(v) => {
                    if t.borrow() == &Token::Literal(LiteralKind::Minus) {
                        Ok(InterpreterValue::Float(-v))
                    } else if t.borrow() == &Token::Literal(LiteralKind::Bang) {
                        Ok(InterpreterValue::Bool(false))
                    } else {
                        Err(miette!("invalid operation {op} for {val}"))?
                    }
                }
                InterpreterValue::Nil => {
                    if t.borrow() == &Token::Literal(LiteralKind::Bang) {
                        Ok(InterpreterValue::Bool(true))
                    } else {
                        Err(miette!("invalid operation {op} for {val}"))?
                    }
                }
                _ => Err(miette!("invalid operation {op} for {val}"))?,
            },
            _ => Err(miette!("invalid operation {op} for {val}"))?,
        }
    }
}
