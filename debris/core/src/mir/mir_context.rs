use std::fmt;

use crate::compile_context::CompilationId;
use crate::mir::mir_nodes::MirNode;
use crate::mir::namespace::MirLocalNamespace;

pub struct MirContext {
    pub id: MirContextId,
    pub nodes: Vec<MirNode>,
    pub local_namespace: MirLocalNamespace,
}

impl MirContext {
    pub fn new(id: MirContextId) -> Self {
        MirContext {
            id,
            nodes: Default::default(),
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
