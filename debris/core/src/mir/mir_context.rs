use std::fmt;

use crate::compile_context::CompilationId;
use crate::mir::mir_nodes::MirNode;
use crate::mir::namespace::MirLocalNamespace;

use super::mir_builder::MirSingletons;
use super::mir_object::MirObjectId;

pub struct MirContext {
    pub id: MirContextId,
    pub super_context_id: Option<MirContextId>,
    pub nodes: Vec<MirNode>,
    pub kind: MirContextKind,
    pub return_values: ReturnValuesData,
    pub return_context: ReturnContext,
    pub local_namespace: MirLocalNamespace,
}

impl MirContext {
    pub fn new(
        id: MirContextId,
        super_context_id: Option<MirContextId>,
        kind: MirContextKind,
        singletons: &MirSingletons,
    ) -> Self {
        MirContext {
            id,
            super_context_id,
            nodes: Default::default(),
            kind,
            return_values: ReturnValuesData::new(kind.default_return_value(singletons)),
            return_context: ReturnContext::Next,
            local_namespace: Default::default(),
        }
    }
}

impl fmt::Debug for MirContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Context {:?}:", self.id)?;

        for node in &self.nodes {
            writeln!(f, "{:?}", node)?;
        }

        writeln!(f)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MirContextKind {
    Block,
    Function,
    Loop,
}

impl MirContextKind {
    pub fn default_return_value(&self, singletons: &MirSingletons) -> MirObjectId {
        match self {
            MirContextKind::Block | MirContextKind::Function => singletons.null,
            MirContextKind::Loop => singletons.never,
        }
    }
}

/// Stores returns values a given context can return
#[derive(Debug)]
pub struct ReturnValuesData {
    pub default_return: MirObjectId,
    pub implicite_return: Option<MirObjectId>,
    pub explicite_returns: Vec<MirObjectId>,
}

impl ReturnValuesData {
    pub fn new(default_return: MirObjectId) -> Self {
        ReturnValuesData {
            default_return,
            implicite_return: Default::default(),
            explicite_returns: Default::default(),
        }
    }

    /// Returns the id of the return value
    /// This should be the first return value in a function
    pub fn return_value(&self) -> MirObjectId {
        self.explicite_returns
            .first()
            .copied()
            .or(self.implicite_return)
            .unwrap_or(self.default_return)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MirContextId {
    pub(super) compilation_id: CompilationId,
    pub(super) id: u32,
}

impl fmt::Debug for MirContextId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.compilation_id.0, self.id)
    }
}

#[derive(Debug)]
pub enum ReturnContext {
    ExitFunction,
    Next,
    Specific(MirContextId),
}
