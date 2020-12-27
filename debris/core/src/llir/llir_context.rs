use std::rc::Rc;

use debris_common::CodeRef;
use generational_arena::Index;

use crate::{
    mir::NamespaceArena,
    mir::{MirNamespaceEntry, MirNode, MirValue},
    namespace::Namespace,
    CompileContext, ObjectRef,
};

/// A specific llir context
///
/// Similar to mir contexts, but a bit simpler.
/// Borrows MirNodes from an actual MirContext.
#[derive(Debug)]
pub(crate) struct LLIRContext<'a, 'code> {
    /// The source code which contains this context
    pub(crate) code: CodeRef<'code>,
    /// The previous mir nodes
    pub(crate) mir_nodes: &'a [MirNode],
    /// All objects
    pub(crate) namespace_idx: Index,
    /// The current context
    pub(crate) compile_context: Rc<CompileContext>,
    /// The id of this context
    pub(crate) context_id: u64,
}

impl<'a> LLIRContext<'a, '_> {
    /// Returns an object that corresponds to a `MirValue`
    ///
    /// If the objects is not yet computed, returns None.
    pub fn get_object(&self, arena: &NamespaceArena, value: &MirValue) -> Option<ObjectRef> {
        match value {
            MirValue::Concrete(obj) => Some(obj.clone()),
            MirValue::Template { id, class: _ } => {
                match self
                    .namespace(arena)
                    .get_by_id(*id)
                    .map(MirNamespaceEntry::value)
                {
                    Some(MirValue::Concrete(obj)) => Some(obj.clone()),
                    Some(MirValue::Template { id: _, class: _ }) | None => None,
                }
            }
        }
    }

    pub fn namespace_mut<'b>(
        &self,
        arena: &'b mut NamespaceArena,
    ) -> &'b mut Namespace<MirNamespaceEntry> {
        &mut arena[self.namespace_idx]
    }

    pub fn namespace<'b>(&self, arena: &'b NamespaceArena) -> &'b Namespace<MirNamespaceEntry> {
        &arena[self.namespace_idx]
    }

    /// Replaces a Template with the given index with an actual value
    ///
    /// Panics if the object is not a template
    pub fn set_object(&self, arena: &mut NamespaceArena, value: ObjectRef, index: u64) {
        let old_value = self
            .namespace_mut(arena)
            .replace_object_at(index, MirNamespaceEntry::Anonymous(value.into()));

        if let MirValue::Concrete(_) = old_value.value() {
            panic!("Expected a template, got a concrete value");
        }
    }
}
