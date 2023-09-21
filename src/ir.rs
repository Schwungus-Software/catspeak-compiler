use std::collections::{HashMap, HashSet};

use color_eyre::eyre::{self, eyre};
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
            scope: vec![],
        }
    }

    pub fn new(interface_preset: impl InterfacePreset) -> Self {
        let mut instance = Self::empty();
        interface_preset.modify(&mut instance);
        instance
    }

    // TODO: document because the method name doesn't tell much.
    pub fn import_global(&mut self, name: &str) {
        // TODO: raise an error when adding an existing global.
        self.globals.insert(name.to_string());
    }

    pub fn produce_ir(mut self, ast: Vec<AstNode>) -> eyre::Result<IR> {
        let main_idx = self.analyze_function(vec![], ast)?;

        let mut ir = IR {
            functions: self.functions,
            entry_points: vec![main_idx],
        };

        ir.optimize();

        Ok(ir)
    }

    fn produce_term(&mut self, node: &AstNode) -> eyre::Result<Term> {
        // TODO: consider a derive macro for automatic conversion.

        let term_kind = match node {
            AstNode::Function { args_list, body } => {
                // TODO: perhaps get rid of cloning by passing slices?
                let body = body.iter().cloned().collect();
                let idx = self.analyze_function(args_list.clone(), body)?;
                TermKind::Function(idx)
            }
            AstNode::Let { var_name, value } => {
                let target = Box::new(
                    self.get_var(var_name)
                        .unwrap_or_else(|| self.create_local_var(var_name)),
                );

                TermKind::Assign {
                    target,
                    value: Box::new(self.produce_term(value)?),
                    operator: AssignOp::Assign,
                }
            }
            AstNode::Assign {
                var_name,
                operator,
                new_value,
            } => {
                let target = Box::new(
                    self.get_var(var_name)
                        .unwrap_or_else(|| self.create_global_var(var_name)),
                );

                TermKind::Assign {
                    target,
                    value: Box::new(self.produce_term(new_value)?),
                    operator: operator.clone(),
                }
            }
            AstNode::If {
                condition,
                body,
                else_block,
            } => TermKind::If {
                condition: Box::new(self.produce_term(condition)?),
                body: Box::new(self.produce_term(body)?),
                else_block: match else_block {
                    None => None,
                    Some(block) => Some(Box::new(self.produce_term(block)?)),
                },
            },
            AstNode::While { condition, body } => TermKind::While {
                condition: Box::new(self.produce_term(condition)?),
                body: Box::new(self.produce_term(body)?),
            },
            AstNode::Block(ast) => {
                let mut terms = vec![];

                for node in ast {
                    terms.push(self.produce_term(node)?);
                }

                TermKind::Block(terms)
            }
            AstNode::Break(value) => {
                let value = Box::new(self.produce_term(value)?);
                TermKind::Break(value)
            }
            AstNode::Funcall { fun_name, args } => {
                let mut terms = vec![];

                for node in args {
                    terms.push(self.produce_term(node)?);
                }

                let callee = Box::new(self.get_var(fun_name).ok_or_else(|| {
                    eyre!("Cannot find function {} in the current scope", fun_name)
                })?);

                TermKind::Funcall {
                    callee,
                    args: terms,
                }
            }
            AstNode::BinaryOp { lhs, rhs, operator } => TermKind::BinaryOp {
                lhs: Box::new(self.produce_term(lhs)?),
                rhs: Box::new(self.produce_term(rhs)?),
                operator: operator.clone(),
            },
            AstNode::UnaryOp { operand, operator } => TermKind::UnaryOp {
                operand: Box::new(self.produce_term(operand)?),
                operator: operator.clone(),
            },
            AstNode::Undefined => TermKind::Value(Value::Undefined),
            AstNode::Bool(value) => TermKind::Value(Value::Bool(*value)),
            AstNode::Real(value) => TermKind::Value(Value::Real(*value)),
            AstNode::String(value) => TermKind::Value(Value::String(value.to_string())),
            AstNode::Ident(name) => {
                return Ok(self
                    .get_var(name)
                    .ok_or_else(|| eyre!("Cannot find variable {} in the current scope", name))?)
            }
        };

        Ok(Term::from(term_kind))
    }

    fn analyze_function(
        &mut self,
        args_list: Vec<String>,
        body: Vec<AstNode>,
    ) -> eyre::Result<usize> {
        let scope = Scope::new(args_list)?;
        self.scope.push(scope);

        let mut terms = vec![];

        for node in body {
            let term = self.produce_term(&node)?;
            terms.push(term);
        }

        let root = if terms.len() == 1 {
            terms[0].clone()
        } else {
            TermKind::Block(terms).into()
        };

        let top = self.scope.last().unwrap();

        let fun = Function {
            local_count: top.locals.len(),
            arg_count: top.args.len(),
            root,
        };

        self.functions.push(fun);

        self.scope.pop();

        Ok(self.functions.len() - 1)
    }

    fn get_var(&mut self, name: &str) -> Option<Term> {
        if self.globals.contains(name) {
            return Some(TermKind::Global(name.to_string()).into());
        }

        let top = self.scope.last_mut().unwrap();

        let idx = if top.locals.contains_key(name) {
            top.locals[name]
        } else if top.args.contains_key(name) {
            top.args[name]
        } else {
            return None;
        };

        Some(TermKind::Local(idx).into())
    }

    fn create_local_var(&mut self, name: &str) -> Term {
        let top = self.scope.last_mut().unwrap();

        let idx = top.args.len() + top.locals.len();
        top.locals.insert(name.to_string(), idx);

        TermKind::Local(idx).into()
    }

    fn create_global_var(&mut self, name: &str) -> Term {
        self.globals.insert(name.to_string());
        TermKind::Global(name.to_string()).into()
    }
}

pub struct Scope {
    args: HashMap<String, usize>,
    locals: HashMap<String, usize>,
}

impl Scope {
    pub fn new(args: Vec<String>) -> eyre::Result<Self> {
        let mut function_args = HashMap::new();

        for name in args {
            if function_args.contains_key(&name) {
                return Err(eyre!("Duplicate function arguments"));
            } else {
                function_args.insert(name, function_args.len());
            }
        }

        let scope = Self {
            args: function_args,
            locals: HashMap::new(),
        };

        Ok(scope)
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
    Function(usize),
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
            Self::Function(idx) => {
                map.serialize_entry("idx", idx)?;
                21
            }
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
        // TODO: do something about the clones all over the place.
        // TODO: perhaps use a proc-macro for the cases of plain tree traversal?
        match &mut self.kind {
            TermKind::Assign {
                target,
                value,
                operator,
            } => {
                return TermKind::Assign {
                    target: Box::new(target.clone().optimize()),
                    value: Box::new(value.clone().optimize()),
                    operator: operator.clone(),
                }
                .into()
            }
            TermKind::Block(terms) => {
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

        self
    }

    fn optimize_block(terms: &mut [Term]) {
        for term in terms {
            *term = term.clone().optimize();
        }
    }
}
