use color_eyre::eyre;
use serde::{Deserialize, Serialize};

use crate::parser::AstNode;

#[derive(Serialize, Deserialize)]
pub struct IR {}

impl IR {
    pub fn parse_from_ast(_ast: Vec<AstNode>) -> eyre::Result<Self> {
        // TODO.
        Ok(Self {})
    }
}
