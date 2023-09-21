use std::io;

use pest::{iterators::Pair, Parser};
use serde::{Deserialize, Serialize};

use crate::eyre;

#[derive(pest_derive::Parser)]
#[grammar = "catspeak.pest"]
pub struct CatspeakParser;

#[derive(Debug, Clone, Serialize)]
pub enum AstNode {
    Let {
        var_name: String,
        value: Box<AstNode>,
    },
    Assign {
        var_name: String,
        operator: AssignOp,
        new_value: Box<AstNode>,
    },
    If {
        condition: Box<AstNode>,
        body: Box<AstNode>,
        else_block: Option<Box<AstNode>>,
    },
    While {
        condition: Box<AstNode>,
        body: Box<AstNode>,
    },
    Break(Box<AstNode>),
    Block(Vec<AstNode>),
    Function {
        args_list: Vec<String>,
        body: Vec<AstNode>,
    },
    Funcall {
        fun_name: String,
        args: Vec<AstNode>,
    },
    BinaryOp {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        operator: BinaryOp,
    },
    UnaryOp {
        operand: Box<AstNode>,
        operator: UnaryOp,
    },
    Undefined,
    Bool(bool),
    Real(f64),
    String(String),
    Ident(String),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Greater,
    Less,
    GreaterEq,
    LessEq,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    BitNot,
    Neg,
    Id,
}

impl AssignOp {
    pub fn idx(&self) -> u8 {
        match self {
            Self::Assign => 0,
            Self::MulAssign => 1,
            Self::DivAssign => 2,
            Self::SubAssign => 3,
            Self::AddAssign => 4,
        }
    }
}

impl BinaryOp {
    pub fn idx(&self) -> u8 {
        match self {
            Self::Mul => 1,
            Self::Div => 2,
            Self::Add => 5,
            Self::Sub => 4,
            Self::Eq => 6,
            Self::Neq => 7,
            Self::Greater => 8,
            Self::GreaterEq => 9,
            Self::Less => 10,
            Self::LessEq => 11,
        }
    }
}

impl UnaryOp {
    pub fn idx(&self) -> u8 {
        match self {
            Self::Not => 12,
            Self::BitNot => 13,
            Self::Neg => BinaryOp::Sub.idx(),
            Self::Id => BinaryOp::Add.idx(),
        }
    }
}

impl CatspeakParser {
    pub fn parse_input(mut input: impl io::Read) -> eyre::Result<Vec<AstNode>> {
        let mut ast = vec![];

        let mut buf = String::new();
        input.read_to_string(&mut buf)?;

        let pairs = CatspeakParser::parse(Rule::script, &buf)?;

        for pair in pairs {
            match pair.as_rule() {
                Rule::stmt => {
                    let stmt = AstNode::parse_stmt(pair);
                    ast.push(stmt);
                }
                Rule::EOI => break,
                _ => unreachable!(),
            }
        }

        debug!("Parsed AST: {:?}", ast);

        Ok(ast)
    }
}

impl AstNode {
    pub fn parse_stmt(pair: Pair<Rule>) -> AstNode {
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::r#let => {
                let mut pair = pair.into_inner();

                let var_name = pair.next().unwrap().as_str().to_owned();
                let value = Self::parse_expr(pair.next().unwrap());

                AstNode::Let {
                    var_name,
                    value: Box::new(value),
                }
            }
            Rule::ass => {
                let mut pair = pair.into_inner();

                let var_name = pair.next().unwrap().as_str().to_owned();

                let operator = match pair.next().unwrap().as_str() {
                    "=" => AssignOp::Assign,
                    "+=" => AssignOp::AddAssign,
                    "-=" => AssignOp::SubAssign,
                    "*=" => AssignOp::MulAssign,
                    "/=" => AssignOp::DivAssign,
                    _ => unreachable!(),
                };

                let new_value = Self::parse_expr(pair.next().unwrap());

                AstNode::Assign {
                    var_name,
                    operator,
                    new_value: Box::new(new_value),
                }
            }
            Rule::r#break => {
                let value = pair.into_inner().next().unwrap();
                let value = Self::parse_expr(value);
                AstNode::Break(Box::new(value))
            }
            Rule::expr => Self::parse_expr(pair),
            _ => unreachable!(),
        }
    }

    pub fn parse_expr_no_bin_op(pair: Pair<Rule>) -> AstNode {
        match pair.as_rule() {
            Rule::binary_op => unreachable!(),
            _ => Self::parse_expr(pair),
        }
    }

    pub fn parse_expr(pair: Pair<Rule>) -> AstNode {
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::r#if => Self::parse_if(pair),
            Rule::r#while => Self::parse_while(pair),
            Rule::block => Self::parse_block(pair),
            Rule::function => Self::parse_function(pair),
            Rule::funcall => Self::parse_funcall(pair),
            Rule::unary_op => Self::parse_unary_op(pair),
            Rule::binary_op => Self::parse_binary_op(pair),
            Rule::paren => Self::parse_paren(pair),
            Rule::value => Self::parse_value(pair),
            _ => unreachable!(),
        }
    }

    pub fn parse_if(pair: Pair<Rule>) -> AstNode {
        let mut pair = pair.into_inner();

        let condition = Box::new(Self::parse_expr(pair.next().unwrap()));
        let body = Box::new(Self::parse_block(pair.next().unwrap()));

        let else_block = pair
            .next()
            .map(|pair| match pair.as_rule() {
                Rule::block => Self::parse_block(pair),
                Rule::r#if => Self::parse_if(pair),
                _ => unreachable!(),
            })
            .map(Box::new);

        AstNode::If {
            condition,
            body,
            else_block,
        }
    }

    pub fn parse_while(pair: Pair<Rule>) -> AstNode {
        let mut pair = pair.into_inner();

        let condition = Box::new(Self::parse_expr(pair.next().unwrap()));
        let body = Box::new(Self::parse_stmt(pair.next().unwrap()));

        AstNode::While { condition, body }
    }

    pub fn parse_block(pair: Pair<Rule>) -> AstNode {
        let mut block = vec![];
        let mut pair = pair.into_inner();

        while let Some(stmt) = pair.next() {
            block.push(Self::parse_stmt(stmt));
        }

        AstNode::Block(block)
    }

    pub fn parse_function(pair: Pair<Rule>) -> AstNode {
        let mut pair = pair.into_inner();

        let mut args_list = vec![];

        let body = loop {
            let pair = pair.next().unwrap();

            match pair.as_rule() {
                Rule::block => {
                    // TODO: move into a method inside `AstNode`.
                    let AstNode::Block(terms) = Self::parse_block(pair) else {
                        unreachable!();
                    };

                    break terms;
                }
                Rule::ident => args_list.push(pair.as_str().to_owned()),
                _ => unreachable!(),
            }
        };

        AstNode::Function { args_list, body }
    }

    pub fn parse_funcall(pair: Pair<Rule>) -> AstNode {
        let mut pair = pair.into_inner();

        let fun_name = pair.next().unwrap().as_str().to_owned();

        let mut args = vec![];

        while let Some(arg) = pair.next() {
            args.push(Self::parse_expr(arg));
        }

        AstNode::Funcall { fun_name, args }
    }

    pub fn parse_binary_op(pair: Pair<Rule>) -> AstNode {
        let mut pair = pair.into_inner();

        let lhs = Box::new(Self::parse_expr_no_bin_op(pair.next().unwrap()));

        let operator = match pair.next().unwrap().as_str() {
            "+" => BinaryOp::Add,
            "-" => BinaryOp::Sub,
            "*" => BinaryOp::Mul,
            "/" => BinaryOp::Div,
            ">" => BinaryOp::Greater,
            "<" => BinaryOp::Less,
            "==" => BinaryOp::Eq,
            ">=" => BinaryOp::GreaterEq,
            "<=" => BinaryOp::LessEq,
            _ => unreachable!(),
        };

        let rhs = Box::new(Self::parse_expr(pair.next().unwrap()));

        AstNode::BinaryOp { lhs, operator, rhs }
    }

    pub fn parse_unary_op(pair: Pair<Rule>) -> AstNode {
        let mut pair = pair.into_inner();

        let operator = match pair.next().unwrap().as_str() {
            "!" => UnaryOp::Not,
            "~" => UnaryOp::BitNot,
            "-" => UnaryOp::Neg,
            "+" => UnaryOp::Id,
            _ => unreachable!(),
        };

        let operand = Box::new(Self::parse_expr_no_bin_op(pair.next().unwrap()));

        AstNode::UnaryOp { operand, operator }
    }

    pub fn parse_paren(pair: Pair<Rule>) -> AstNode {
        Self::parse_expr(pair.into_inner().next().unwrap())
    }

    pub fn parse_value(pair: Pair<Rule>) -> AstNode {
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::undefined => AstNode::Undefined,
            Rule::bool => {
                let value = pair.as_str() == "true";
                AstNode::Bool(value)
            }
            Rule::ident => {
                let ident = pair.as_str().to_owned();
                AstNode::Ident(ident)
            }
            Rule::number => {
                let string = pair.as_str();

                let real = string.parse::<f64>().unwrap_or_else(|_| {
                    let integer: i64 = string.parse().unwrap();
                    integer as f64
                });

                AstNode::Real(real)
            }
            Rule::string => {
                let mut pair = pair.into_inner();
                let mut buf = String::new();

                while let Some(token) = pair.next() {
                    match token.as_rule() {
                        Rule::escape => {
                            let c = match &token.as_str()[1..] {
                                "n" => '\n',
                                // TODO: implement more escape sequences.
                                _ => unreachable!(),
                            };

                            buf.push(c);
                        }
                        Rule::char => buf.push_str(token.as_str()),
                        _ => unreachable!(),
                    }
                }

                AstNode::String(buf)
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use super::*;

    #[test]
    fn parse_big_file() -> eyre::Result<()> {
        let file = File::open("testfiles/insanity.txt")?;
        CatspeakParser::parse_input(&file)?;
        Ok(())
    }
}
