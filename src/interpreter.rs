use std::fmt::{Display, Formatter};
use crate::{error::InterpreterResult, parser::Ast};

pub struct Interpreter {
    ast: Ast,
}

impl Interpreter {
    pub fn new(ast: Ast) -> Self {
        Interpreter { ast }
    }

    pub fn interpret(self) -> InterpreterResult<ExprResult> {
        interpret_node(&self.ast, &[])
    }
}

fn interpret_node(ast: &Ast, rest: &[Ast]) -> InterpreterResult<ExprResult> {
    eprintln!("interpreting {}", ast);
    let exp = match ast {
        Ast::Atom(a) => match a {
            crate::parser::AstToken::Number(n) => ExprResult::Number(*n),
            crate::parser::AstToken::String(s) => ExprResult::String(s.clone()),
            crate::parser::AstToken::Expr(e) => match e {
                crate::parser::ExprType::False => ExprResult::Boolean(false),
                crate::parser::ExprType::True => ExprResult::Boolean(true),
                crate::parser::ExprType::Nil => ExprResult::Nil,
                crate::parser::ExprType::Minus => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    ExprResult::Number(left - right)
                }
                crate::parser::ExprType::Plus => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let rhs = interpret_node(&rest[1], &[])?;
                    match lhs {
                        ExprResult::Number(left) => {
                            let right = match rhs {
                                ExprResult::Number(n) => n,
                                _ => todo!("invalid operand for add {rhs}"),
                            };

                            ExprResult::Number(left + right)

                        }
                        ExprResult::String(left) => {
                            let right = match rhs {
                                ExprResult::String(s) => s,
                                _ => todo!("invalid operand for add {rhs}"),
                            };

                            let mut concat = String::from(left);
                            concat.push_str(right.as_str());
                         
                            ExprResult::String(concat)
                        }
                        _ => todo!("invalid operand for addition {lhs}"),
                    }
                }
                crate::parser::ExprType::Star => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };
                    
                    ExprResult::Number(left * right)
                }
                crate::parser::ExprType::Slash => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    ExprResult::Number(left / right)
                }
                crate::parser::ExprType::EqEq => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    ExprResult::Boolean(left == right)
                }
                crate::parser::ExprType::Bang => {
                    let rhs = interpret_node(&rest[0], &[])?;
                    let right = match rhs {
                        ExprResult::Boolean(b) => b,
                        _ => todo!()
                    };

                    ExprResult::Boolean(!right)
                }
                crate::parser::ExprType::BangEq => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    ExprResult::Boolean(left != right)
                }
                crate::parser::ExprType::Less => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    ExprResult::Boolean(left < right)
                }
                crate::parser::ExprType::LessEq => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    ExprResult::Boolean(left <= right)
                }
                crate::parser::ExprType::Greater => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    ExprResult::Boolean(left > right)
                }
                crate::parser::ExprType::GreaterEq => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!()
                    };

                    ExprResult::Boolean(left >= right)
                }
                crate::parser::ExprType::And => todo!(),
                crate::parser::ExprType::Or => todo!(),
                crate::parser::ExprType::Group(_, _) => todo!(),
                _ => ExprResult::Err,
            },
            _ => ExprResult::Err,
        },
        Ast::Cons(a, r) => interpret_node(a, r)?,
    };

    Ok(exp)
}

#[derive(Debug)]
pub enum ExprResult {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
    Err,
}

impl Display for ExprResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprResult::Number(n) => write!(f, "{n}"),
            ExprResult::String(s) => write!(f, "{s}"),
            ExprResult::Boolean(b) => write!(f, "{b}"),
            ExprResult::Nil => write!(f, "nil"),
            ExprResult::Err => write!(f, "err"),
        }
    }
}
