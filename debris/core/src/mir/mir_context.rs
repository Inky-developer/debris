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
    pub return_values_id: ReturnValuesDataId,
    pub return_context: ReturnContext,
    pub local_namespace: MirLocalNamespace,
}

impl MirContext {
    pub fn new(
        id: MirContextId,
        super_context_id: Option<MirContextId>,
        kind: MirContextKind,
        return_values_id: ReturnValuesDataId,
        return_context: ReturnContext,
    ) -> Self {
        MirContext {
            id,
            super_context_id,
            nodes: Default::default(),
            kind,
            return_values_id,
            return_context,
            local_namespace: Default::default(),
        }
    }
}

impl MirContext {
    pub fn return_values<'a>(&self, arena: &'a ReturnValuesArena) -> &'a ReturnValuesData {
        arena.get(self.return_values_id)
    }

    pub fn return_values_mut<'a>(&self, arena: &'a mut ReturnValuesArena) -> &'a mut ReturnValuesData {
        arena.get_mut(self.return_values_id)
    }
}

impl fmt::Debug for MirContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Context {:?}:", self.id,)?;

        for node in &self.nodes {
            writeln!(f, "{:?}", node)?;
        }

        match self.return_context {
            ReturnContext::Specific(context_id) => writeln!(f, "Next Context: {:?}", context_id),
            ReturnContext::Pass => writeln!(f, "Return")
        }?;

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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ReturnValuesDataId(usize);

#[derive(Debug, Default)]
pub struct ReturnValuesArena {
    return_values: Vec<ReturnValuesData>,
}

impl ReturnValuesArena {
    pub fn add(&mut self, return_values_data: ReturnValuesData) -> ReturnValuesDataId {
        self.return_values.push(return_values_data);
        ReturnValuesDataId(self.return_values.len() - 1)
    }

    pub fn get(&self, id: ReturnValuesDataId) -> &ReturnValuesData {
        &self.return_values[id.0]
    }

    pub fn get_mut(&mut self, id: ReturnValuesDataId) -> &mut ReturnValuesData {
        &mut self.return_values[id.0]
    }
}

/// Stores returns values a given context can return
/// Multiple contexts can share the same return values data
#[derive(Debug)]
pub struct ReturnValuesData {
    pub default_return: MirObjectId,
    pub explicite_return: Option<MirObjectId>,
    pub unconditionally_returned: bool,
}

impl ReturnValuesData {
    pub fn new(default_return: MirObjectId) -> Self {
        ReturnValuesData {
            default_return,
            explicite_return: None,
            unconditionally_returned: false,
        }
    }

    /// Returns the id of the return value
    /// This should be the first return value in a function
    pub fn return_value(&self) -> MirObjectId {
        self.explicite_return.unwrap_or(self.default_return)
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

/// Specifies where to jump to when all commands of a context are run
#[derive(Debug, Clone, Copy)]
pub enum ReturnContext {
    /// Don't jump anywhere, just return to the caller
    Pass,
    /// Jump to a specific context
    Specific(MirContextId),
}
