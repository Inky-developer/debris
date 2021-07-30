use crate::compile_context::CompilationId;
use crate::mir::namespace::MirLocalNamespace;

/// A duck-typed object. A MirObject contains all attributes that it needs to have in order
/// to compile.
#[derive(Debug)]
pub struct MirObject {
    pub id: MirObjectId,
    pub local_namespace: MirLocalNamespace,
}

impl MirObject {
    pub fn new(id: MirObjectId) -> Self {
        MirObject {
            id,
            local_namespace: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MirObjectId {
    compilation_id: CompilationId,
    id: u32,
}

impl MirObjectId {
    pub(super) fn new(compilation_id: CompilationId, id: u32) -> Self {
        MirObjectId { compilation_id, id }
    }
}
