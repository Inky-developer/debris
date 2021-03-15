use debris_common::Span;

use crate::{
    mir::NamespaceArena,
    mir::{ContextId, MirContextMap, MirNode, MirValue},
    namespace::NamespaceEntry,
    CompileContext, ObjectRef,
};

use super::utils::ItemId;

/// A specific llir context
///
/// Similar to mir contexts, but a bit simpler.
/// Borrows MirNodes from an actual MirContext.
#[derive(Debug)]
pub struct LlirContext<'ctx> {
    pub span: Span,
    /// The previous mir nodes
    pub(crate) mir_nodes: &'ctx [MirNode],
    /// The current context
    pub(crate) compile_context: &'ctx CompileContext,
    /// The id of this context
    pub(crate) context_id: ContextId,
}

impl<'ctx> LlirContext<'ctx> {
    /// Returns an object that corresponds to a `MirValue`
    ///
    /// If the objects is not yet computed, returns None.
    pub fn get_object(
        &self,
        arena: &NamespaceArena,
        contexts: &MirContextMap,
        value: &MirValue,
    ) -> Option<ObjectRef> {
        match value {
            MirValue::Concrete(obj) => Some(obj.clone()),
            MirValue::Template { id, class: _ } => {
                let context = contexts.get(id.context);
                arena
                    .get_by_id(id.id, context.id.as_inner())
                    .and_then(|val| val.concrete())
            }
        }
    }

    /// Replaces a Template with the given index with an actual value
    ///
    /// Panics if the object already exists
    pub fn set_object(
        &self,
        arena: &mut NamespaceArena,
        contexts: &MirContextMap,
        value: ObjectRef,
        index: ItemId,
    ) {
        let context = contexts.get(index.context);

        let old_value = arena.replace_with_id(
            index.id,
            context.id.as_inner(),
            NamespaceEntry::Anonymous(value.into()),
        );

        if let MirValue::Concrete(_) = old_value.value() {
            panic!("Template already replaced by concrete value");
        }
    }

    pub fn replace_object(
        &self,
        arena: &mut NamespaceArena,
        contexts: &MirContextMap,
        value: ObjectRef,
        index: ItemId,
    ) {
        let context = contexts.get(index.context);

        arena.replace_with_id(
            index.id,
            context.id.as_inner(),
            NamespaceEntry::Anonymous(value.into()),
        );
    }
}
