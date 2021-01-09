use debris_common::CodeRef;

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
pub(crate) struct LLIRContext<'ctx> {
    /// The source code which contains this context
    pub(crate) code: CodeRef<'ctx>,
    /// The previous mir nodes
    pub(crate) mir_nodes: &'ctx [MirNode],
    /// The current context
    pub(crate) compile_context: &'ctx CompileContext,
    /// The id of this context
    pub(crate) context_id: ContextId,
}

impl<'ctx> LLIRContext<'ctx> {
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
    /// Panics if the object is not a template
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
            panic!("Template already replace by concrete value");
        }
    }
}
