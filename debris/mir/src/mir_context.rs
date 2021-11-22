use std::fmt;

use debris_common::{CompilationId, Span};

use crate::{
    mir_builder::MirSingletons,
    mir_nodes::MirNode,
    mir_object::MirObjectId,
    namespace::{MirLocalNamespace, MirLocalNamespaceId, MirNamespace},
};

pub struct MirContext {
    pub id: MirContextId,
    /// The parent context of this context
    pub super_context_id: Option<MirContextId>,
    pub nodes: Vec<MirNode>,
    pub kind: MirContextKind,
    pub return_values_id: ReturnValuesDataId,
    pub return_context: ReturnContext,
    pub local_namespace_id: MirLocalNamespaceId,
}

impl MirContext {
    pub fn new(
        id: MirContextId,
        super_context_id: Option<MirContextId>,
        local_namespace_id: MirLocalNamespaceId,
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
            local_namespace_id,
        }
    }

    pub fn local_namespace<'a>(
        &self,
        global_namespace: &'a mut MirNamespace,
    ) -> &'a mut MirLocalNamespace {
        global_namespace.get_local_namespace(self.local_namespace_id)
    }
}

impl MirContext {
    pub fn return_values<'a>(&self, arena: &'a ReturnValuesArena) -> &'a ReturnValuesData {
        arena.get(self.return_values_id)
    }

    pub fn return_values_mut<'a>(
        &self,
        arena: &'a mut ReturnValuesArena,
    ) -> &'a mut ReturnValuesData {
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
            ReturnContext::ManuallyHandled(info_context_id) => {
                writeln!(f, "Return (Default is {:?})", info_context_id)
            }
            ReturnContext::Pass => writeln!(f, "Return"),
        }?;

        writeln!(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MirContextKind {
    Block,
    Module,
    Function,
    Loop,
}

impl MirContextKind {
    pub fn default_return_value(&self, singletons: &MirSingletons) -> MirObjectId {
        match self {
            MirContextKind::Block | MirContextKind::Function | MirContextKind::Module => {
                singletons.null
            }
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
    /// The id of the explicitely returned value and the span where it was declared
    pub explicite_return: Option<(MirObjectId, Span)>,
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
        self.explicite_return
            .map_or(self.default_return, |(obj_id, _)| obj_id)
    }

    pub fn return_span(&self) -> Option<Span> {
        self.explicite_return.as_ref().map(|(_, span)| *span)
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
    /// Don't jump anywhere, because the jump was handled manually.
    /// This is used to store some information about the target return context.
    ManuallyHandled(MirContextId),
    /// Jump to a specific context
    Specific(MirContextId),
}

impl ReturnContext {
    pub fn set_handled_manually(&mut self) {
        if let ReturnContext::Specific(id) = self {
            *self = ReturnContext::ManuallyHandled(*id);
        }
    }
}
