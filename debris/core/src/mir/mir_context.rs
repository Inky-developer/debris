use crate::compile_context::CompilationId;
use crate::mir::mir_nodes::MirNode;
use crate::mir::namespace::MirLocalNamespace;

#[derive(Debug)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MirContextId {
    pub(super) compilation_id: CompilationId,
    pub(super) id: u32,
}
