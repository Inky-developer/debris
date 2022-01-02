use std::fmt;

use debris_common::{CompilationId, Ident, Span};

use crate::{
    mir_context::MirContextId,
    mir_nodes::{MirNode, PropertyAccess},
    namespace::{MirLocalNamespaceId, MirNamespace},
};

/// A duck-typed object. A [`MirObject`] contains all attributes that it needs to have in order
/// to compile.
#[derive(Debug)]
pub struct MirObject {
    pub id: MirObjectId,
    pub defining_context: MirContextId,
    pub local_namespace_id: MirLocalNamespaceId,
}

impl MirObject {
    pub fn new_in(namespace: &mut MirNamespace, context: MirContextId, id: MirObjectId) -> Self {
        MirObject {
            id,
            defining_context: context,
            local_namespace_id: namespace.insert_local_namespace(),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MirObjectId {
    pub(super) compilation_id: CompilationId,
    pub(super) id: u32,
}

impl MirObjectId {
    pub(super) fn new(compilation_id: CompilationId, id: u32) -> Self {
        MirObjectId { compilation_id, id }
    }

    pub fn property_get_or_insert(
        self,
        global_namespace: &mut MirNamespace,
        nodes: &mut Vec<MirNode>,
        ident: Ident,
        span: Span,
        current_context_id: MirContextId,
    ) -> MirObjectId {
        let namespace_id = global_namespace.get_obj(self).local_namespace_id;
        let namespace = global_namespace.get_local_namespace(namespace_id);
        if let Some(obj_id) = namespace.get_property(&ident) {
            return obj_id;
        }

        let new_obj = global_namespace.insert_object(current_context_id).id;
        nodes.push(MirNode::PropertyAccess(PropertyAccess {
            span,
            target_id: new_obj,
            property_ident: ident.clone(),
            value_id: self,
        }));
        global_namespace
            .get_local_namespace_mut(namespace_id)
            .insert(new_obj, ident, span);
        new_obj
    }

    pub fn get_property(
        self,
        global_namespace: &MirNamespace,
        ident: &Ident,
    ) -> Option<MirObjectId> {
        let namespace_id = global_namespace.get_obj(self).local_namespace_id;
        global_namespace
            .get_local_namespace(namespace_id)
            .get_property(ident)
    }
}

impl fmt::Debug for MirObjectId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.compilation_id.0, self.id)
    }
}
