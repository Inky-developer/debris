use std::fmt;

use crate::compile_context::CompilationId;
use crate::mir::mir_nodes::MirNode;
use crate::mir::namespace::MirLocalNamespace;

use super::mir_object::MirObjectId;

pub struct MirContext {
    pub id: MirContextId,
    pub super_context_id: Option<MirContextId>,
    pub nodes: Vec<MirNode>,
    pub return_value: Option<MirObjectId>,
    pub local_namespace: MirLocalNamespace,
}

impl MirContext {
    pub fn new(id: MirContextId, super_context_id: Option<MirContextId>) -> Self {
        MirContext {
            id,
            super_context_id,
            nodes: Default::default(),
            return_value: None,
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
