use std::collections::{HashMap, HashSet};

use serde::{ser::SerializeMap, Serialize};

use crate::parser::{AssignOp, AstNode, BinaryOp, UnaryOp};

pub trait InterfacePreset {
    fn modify(&self, script: &mut Script);
}

pub struct Script {
    functions: Vec<Function>,
    globals: HashSet<String>,
    scope: Vec<Scope>,
}

impl Script {
    pub fn empty() -> Self {
        Self {
            functions: vec![],
            globals: HashSet::new(),
            scope: vec![Scope::empty()],
        }
    }

    pub fn new(interface_preset: impl InterfacePreset) -> Self {
        let mut instance = Self::empty();
        interface_preset.modify(&mut instance);
        instance
    }

    // TODO: document because the method name doesn't tell much.
    pub fn import_global(&mut self, name: &str) {
        self.globals.insert(name.to_string());
    }

    pub fn produce_ir(mut self, ast: Vec<AstNode>) -> IR {
        let main = self.analyze_root(ast);
        self.functions.insert(0, main);

        let mut ir = IR {
            functions: self.functions,
            entry_points: vec![0],
        };

        ir.optimize();

        ir
    }

    fn produce_term(&mut self, node: &AstNode) -> Term {
        // TODO: consider a derive macro for automatic conversion.

        let term = match node {
            AstNode::Let { var_name, value } => TermKind::Assign {
                target: Box::new(self.create_var(var_name)),
                value: Box::new(self.produce_term(value)),
                operator: AssignOp::Assign,
            },
            AstNode::Assign {
                var_name,
                operator,
                new_value,
            } => TermKind::Assign {
                target: Box::new(self.create_var(var_name)),
                value: Box::new(self.produce_term(new_value)),
                operator: operator.clone(),
            },
            AstNode::If {
                condition,
                body,
                else_block,
            } => TermKind::If {
                condition: Box::new(self.produce_term(condition)),
                body: Box::new(self.produce_term(body)),
                else_block: else_block
                    .as_ref()
                    .map(|x| self.produce_term(x))
                    .map(Box::new),
            },
            AstNode::While { condition, body } => TermKind::While {
                condition: Box::new(self.produce_term(condition)),
                body: Box::new(self.produce_term(body)),
            },
            AstNode::Block(ast) => {
                let terms = ast.iter().map(|x| self.produce_term(x)).collect();
                TermKind::Block(terms)
            }
            AstNode::Break(value) => {
                let value = Box::new(self.produce_term(value));
                TermKind::Break(value)
            }
            AstNode::Funcall { fun_name, args } => {
                let args = args.iter().map(|x| self.produce_term(x)).collect();

                TermKind::Funcall {
                    callee: Box::new(self.create_var(fun_name)),
                    args,
                }
            }
            AstNode::BinaryOp { lhs, rhs, operator } => TermKind::BinaryOp {
                lhs: Box::new(self.produce_term(lhs)),
                rhs: Box::new(self.produce_term(rhs)),
                operator: operator.clone(),
            },
            AstNode::UnaryOp { operand, operator } => TermKind::UnaryOp {
                operand: Box::new(self.produce_term(operand)),
                operator: operator.clone(),
            },
            AstNode::Undefined => TermKind::Value(Value::Undefined),
            AstNode::Bool(value) => TermKind::Value(Value::Bool(*value)),
            AstNode::Real(value) => TermKind::Value(Value::Real(*value)),
            AstNode::String(value) => TermKind::Value(Value::String(value.to_string())),
            AstNode::Ident(name) => return self.create_var(name),
        };

        term.into()
    }

    fn analyze_root(&mut self, ast: Vec<AstNode>) -> Function {
        let mut terms = vec![];

        for node in ast {
            let term = self.produce_term(&node);
            terms.push(term);
        }

        let root = if terms.len() == 1 {
            terms[0].clone()
        } else {
            TermKind::Block(terms).into()
        };

        let top = self.scope.last().unwrap();

        Function {
            local_count: top.locals.len(),
            arg_count: 0,
            root,
        }
    }

    fn create_var(&mut self, name: &str) -> Term {
        if self.globals.contains(name) {
            return TermKind::Global(name.to_string()).into();
        }

        for frame in self.scope.iter().rev() {
            if frame.locals.contains_key(name) {
                let idx = frame.locals[name];
                return TermKind::Local(idx).into();
            }
        }

        let top = self.scope.last_mut().unwrap();
        top.locals.insert(name.to_string(), top.locals.len());
        TermKind::Local(top.locals.len() - 1).into()
    }
}

pub struct Scope {
    locals: HashMap<String, usize>,
}

impl Scope {
    pub fn empty() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct IR {
    functions: Vec<Function>,
    entry_points: Vec<usize>,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Function {
    local_count: usize,
    arg_count: usize,
    root: Term,
}

#[derive(Serialize, Debug, Clone)]
pub struct Term {
    dbg: Option<u32>, // TODO: figure out the format
    #[serde(flatten)]
    kind: TermKind,
}

impl From<TermKind> for Term {
    fn from(value: TermKind) -> Self {
        Self {
            dbg: None,
            kind: value,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TermKind {
    Local(usize),
    Global(String),
    Assign {
        target: Box<Term>,
        value: Box<Term>,
        operator: AssignOp,
    },
    Value(Value),
    Block(Vec<Term>),
    Break(Box<Term>),
    Funcall {
        callee: Box<Term>,
        args: Vec<Term>,
    },
    If {
        condition: Box<Term>,
        body: Box<Term>,
        else_block: Option<Box<Term>>,
    },
    While {
        condition: Box<Term>,
        body: Box<Term>,
    },
    BinaryOp {
        lhs: Box<Term>,
        rhs: Box<Term>,
        operator: BinaryOp,
    },
    UnaryOp {
        operand: Box<Term>,
        operator: UnaryOp,
    },
}

#[derive(Serialize, Debug, Clone)]
#[serde(untagged)]
pub enum Value {
    #[serde(serialize_with = "serialize_none_value")]
    Undefined,
    Bool(bool),
    Real(f64),
    String(String),
}

fn serialize_none_value<S>(serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_none()
}

// TODO: look into a declarative solution with the `serde` attribute.
impl Serialize for TermKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut map = serializer.serialize_map(None)?;

        let type_idx = match self {
            Self::Funcall { callee, args } => {
                map.serialize_entry("callee", callee)?;
                map.serialize_entry("args", args)?;

                14
            }
            Self::Local(idx) => {
                map.serialize_entry("idx", &(*idx as f64))?;
                19
            }
            Self::Global(name) => {
                map.serialize_entry("name", &name)?;
                20
            }
            Self::Value(value) => {
                map.serialize_entry("value", &value)?;
                0
            }
            Self::Break(value) => {
                map.serialize_entry("value", &value)?;
                10
            }
            Self::Assign {
                target,
                value,
                operator,
            } => {
                map.serialize_entry("assignType", &operator.idx())?;
                map.serialize_entry("target", target)?;
                map.serialize_entry("value", value)?;

                16
            }
            Self::Block(terms) => {
                map.serialize_entry("terms", terms)?;
                3
            }
            Self::BinaryOp { lhs, rhs, operator } => {
                map.serialize_entry("operator", &(operator.idx() as f64))?;
                map.serialize_entry("lhs", lhs)?;
                map.serialize_entry("rhs", rhs)?;

                12
            }
            Self::UnaryOp { operand, operator } => {
                map.serialize_entry("operator", &(operator.idx() as f64))?;
                map.serialize_entry("value", operand)?;

                13
            }
            Self::If {
                condition,
                body,
                else_block,
            } => {
                map.serialize_entry("condition", condition)?;
                map.serialize_entry("ifTrue", body)?;
                map.serialize_entry("ifFalse", else_block)?;

                4
            }
            Self::While { condition, body } => {
                map.serialize_entry("condition", condition)?;
                map.serialize_entry("body", body)?;

                7
            }
        };

        map.serialize_entry("type", &type_idx)?;

        map.end()
    }
}

impl IR {
    fn optimize(&mut self) {
        for fun in &mut self.functions {
            fun.root = fun.root.clone().optimize();
        }
    }
}

impl Term {
    fn optimize(mut self) -> Term {
        match &mut self.kind {
            TermKind::Block(terms) => {
                // HACK: workaround for an empty block crashing the IR loader by indexing -1 from `terms`.
                if terms.is_empty() {
                    return TermKind::Value(Value::Undefined).into();
                }

                Self::optimize_block(terms);

                if terms.len() == 1 {
                    return terms[0].clone();
                }
            }
            TermKind::If {
                condition,
                body,
                else_block,
            } => {
                return TermKind::If {
                    condition: Box::new(condition.clone().optimize()),
                    body: Box::new(body.clone().optimize()),
                    else_block: else_block.clone().map(|x| x.optimize()).map(Box::new),
                }
                .into();
            }
            _ => (),
        };

        return self;
    }

    fn optimize_block(terms: &mut [Term]) {
        for term in terms {
            *term = term.clone().optimize();
        }
    }
}
