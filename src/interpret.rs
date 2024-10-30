use crate::parser::Ast;
use crate::parser::Expr;
use crate::parser::Node;
use crate::token::{KeywordKind, LiteralKind, Token};
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};

type InterpreterResult = Result<InterpreterValue, miette::Error>;

pub struct Interpreter {
    ast: Ast,
}

#[derive(Debug)]
pub enum InterpreterValue {
    BoolVal(bool),
    FloatVal(f64),
    StringVal(String),
}

impl Display for InterpreterValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterValue::BoolVal(v) => write!(f, "{v}"),
            InterpreterValue::FloatVal(v) => write!(f, "{v}"),
            InterpreterValue::StringVal(v) => write!(f, "{v}"),
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
                Expr::Unary(_op, _exp) => todo!(),
                Expr::Group(group_exp) => self.evaluate_all(group_exp),
            },
        }
    }

    fn eval_literal(token: &Token) -> InterpreterResult {
        match token {
            Token::Number { value, .. } => Ok(InterpreterValue::FloatVal(*value)),
            Token::String { value } => Ok(InterpreterValue::StringVal(value.clone())),
            Token::Keyword(KeywordKind::True) => Ok(InterpreterValue::BoolVal(true)),
            Token::Keyword(KeywordKind::False) => Ok(InterpreterValue::BoolVal(false)),
            _ => unimplemented!("{:?}", token),
        }
    }

    fn eval_binary_exp(&self, left: &Box<Node>, op: &Node, right: &Box<Node>) -> InterpreterResult {
        let left = self.evaluate_all(left)?;
        let right = self.evaluate_all(right)?;
        match op {
            Node::Terminal(t) => match t {
                Token::Literal(l) => match l {
                    LiteralKind::Plus => {
                        match left {
                            InterpreterValue::FloatVal(f_l) => match right {
                                InterpreterValue::FloatVal(f_r) => {
                                    Ok(InterpreterValue::FloatVal(f_l + f_r))
                                }
                                _ => {
                                    // todo: to take advantage of miette would be nice to have row/col info here
                                    Err(miette::miette!(
                                        "type mismatch - can't add {left} to {right}"
                                    ))
                                }
                            },
                            InterpreterValue::StringVal(ref s_l) => match right {
                                InterpreterValue::StringVal(s_r) => {
                                    let mut s = String::from(s_l);
                                    s.push_str(s_r.as_str());
                                    Ok(InterpreterValue::StringVal(s))
                                }
                                _ => {
                                    // todo: to take advantage of miette would be nice to have row/col info here
                                    Err(miette::miette!(
                                        "type mismatch - can't add {left} to {right}"
                                    ))
                                }
                            },
                            InterpreterValue::BoolVal(_) => {
                                // todo: to take advantage of miette would be nice to have row/col info here
                                Err(miette::miette!("invalid operation {op} for {left}"))
                            }
                        }
                    }
                    // LiteralKind::Minus => {}
                    // LiteralKind::Star => {}
                    // LiteralKind::Slash => {}
                    // LiteralKind::EqEq => {}
                    // LiteralKind::BangEq => {}
                    // LiteralKind::Less => {}
                    // LiteralKind::LessEq => {}
                    // LiteralKind::Greater => {}
                    // LiteralKind::GreaterEq => {}
                    _ => todo!("invalid {l}"),
                },
                _ => todo!("invalid {t}"),
            },
            _ => todo!("invalid {op}"),
        }
    }
}
