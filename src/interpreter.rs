use crate::{error::InterpreterResult, parser::Ast};
use std::fmt::{Display, Formatter};
use crate::parser::{AstToken, ExprType};

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
    eprintln!("interpret_node({}, {:?})", ast, rest);
    let exp = match ast {
        Ast::Atom(a) => match a {
            AstToken::Number(n) => ExprResult::Number(*n),
            AstToken::String(s) => ExprResult::String(s.clone()),
            AstToken::Expr(e) => match e {
                ExprType::False => ExprResult::Boolean(false),
                ExprType::True => ExprResult::Boolean(true),
                ExprType::Nil => ExprResult::Nil,
                ExprType::Minus => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Number(left - right)
                }
                ExprType::Plus => {
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

                            let mut concat = left;
                            concat.push_str(right.as_str());

                            ExprResult::String(concat)
                        }
                        _ => todo!("invalid operand for addition {lhs}"),
                    }
                }
                ExprType::Star => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Number(left * right)
                }
                ExprType::Slash => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Number(left / right)
                }
                ExprType::EqEq => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Boolean(left == right)
                }
                ExprType::Bang => {
                    let rhs = interpret_node(&rest[0], &[])?;
                    let right = match rhs {
                        ExprResult::Boolean(b) => b,
                        _ => todo!(),
                    };

                    ExprResult::Boolean(!right)
                }
                ExprType::BangEq => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Boolean(left != right)
                }
                ExprType::Less => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Boolean(left < right)
                }
                ExprType::LessEq => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Boolean(left <= right)
                }
                ExprType::Greater => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Boolean(left > right)
                }
                ExprType::GreaterEq => {
                    let lhs = interpret_node(&rest[0], &[])?;
                    let left = match lhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    let rhs = interpret_node(&rest[1], &[])?;
                    let right = match rhs {
                        ExprResult::Number(n) => n,
                        _ => todo!(),
                    };

                    ExprResult::Boolean(left >= right)
                }
                ExprType::And => todo!(),
                ExprType::Or => todo!(),
                _ => {
                    eprintln!("expression {}", e);
                    ExprResult::Err(ast.clone())
                },
            },
            AstToken::GroupStart => interpret_node(&rest[0], &rest[1..])?,  
            AstToken::GroupEnd => panic!("should never see a GroupEnd"),
            _ => {
                eprintln!("ast {}", ast);
                ExprResult::Err(ast.clone()) 
            },
        },
        Ast::Cons(a, r) => {
            eprintln!("cons({a}, {r:?})");
            if **a != Ast::Atom(AstToken::GroupStart) {
                interpret_node(&r[0], &r[1..])?
            } else {
                interpret_node(a, r)?
            }
        },
    };

    Ok(exp)
}

#[derive(Debug)]
pub enum ExprResult {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
    Err(Ast),
}

impl Display for ExprResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprResult::Number(n) => write!(f, "{n}"),
            ExprResult::String(s) => write!(f, "{s}"),
            ExprResult::Boolean(b) => write!(f, "{b}"),
            ExprResult::Nil => write!(f, "nil"),
            ExprResult::Err(ast) => write!(f, "invalid token: {ast}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{AstToken, ExprType};
    use super::*;
   
    #[test]
    fn group_true() -> InterpreterResult<()> {
        let ast = Ast::Cons(Box::new(Ast::Atom(AstToken::GroupStart)), vec![Ast::Atom(AstToken::Expr(ExprType::True))]);
        let interpreter = Interpreter::new(ast);
        let res = interpreter.interpret()?;
        
        assert_eq!(res.to_string(), "true");
        
        Ok(())
    }

}