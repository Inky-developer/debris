use rustc_hash::FxHashMap;

use debris_common::{CompilationId, CompileContext, Ident, Span};

use crate::{
    mir_context::MirContextId,
    mir_object::{MirObject, MirObjectId},
};

#[derive(Debug)]
pub struct MirNamespace {
    /// The objects are indexed by `MirObjectId.id`
    /// For this reason, no object may ever be removed from this vec
    objects: Vec<MirObject>,
    /// This contains some information about objects which might not be defined
    /// The span is the span where this object is first used.
    /// These metadata are used in the llir stage, where an error is thrown when the
    /// llir builder tries to access a not-initialized object.
    maybe_erroneous_objects: FxHashMap<MirObjectId, (Ident, Span)>,
    compilation_id: CompilationId,
    local_namespaces: Vec<MirLocalNamespace>,
}

impl MirNamespace {
    pub fn new(ctx: &CompileContext) -> Self {
        MirNamespace {
            objects: Vec::new(),
            maybe_erroneous_objects: Default::default(),
            local_namespaces: Vec::new(),
            compilation_id: ctx.compilation_id,
        }
    }

    /// Creates a new object. This object stores the current context id as the place where it was defined
    pub fn insert_object(&mut self, current_context_id: MirContextId) -> &mut MirObject {
        let id = MirObjectId::new(self.compilation_id, self.objects.len() as u32);
        let object = MirObject::new_in(current_context_id, id);
        self.objects.push(object);
        self.objects.last_mut().unwrap()
    }

    pub fn get_obj_mut(&mut self, obj: MirObjectId) -> &mut MirObject {
        &mut self.objects[obj.id as usize]
    }

    pub fn get_obj(&self, obj: MirObjectId) -> &MirObject {
        &self.objects[obj.id as usize]
    }

    /// Adds some metadata for objects which might not be correctly defined.
    pub fn add_maybe_erroneous_obj_info(&mut self, obj_id: MirObjectId, data: (Ident, Span)) {
        self.maybe_erroneous_objects.insert(obj_id, data);
    }

    /// Returns info for the object with `obj_id`, if it has any
    pub fn get_obj_info(&self, obj_id: MirObjectId) -> Option<&(Ident, Span)> {
        self.maybe_erroneous_objects.get(&obj_id)
    }

    pub fn insert_local_namespace(&mut self) -> MirLocalNamespaceId {
        let id = self.local_namespaces.len();
        let namespace = MirLocalNamespace::default();
        self.local_namespaces.push(namespace);
        MirLocalNamespaceId(id)
    }

    pub fn get_local_namespace(&self, id: MirLocalNamespaceId) -> &MirLocalNamespace {
        &self.local_namespaces[id.0]
    }

    pub fn get_local_namespace_mut(&mut self, id: MirLocalNamespaceId) -> &mut MirLocalNamespace {
        &mut self.local_namespaces[id.0]
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct MirLocalNamespaceId(usize);

/// A tuple of the actual object and the span where it was first declared
type LocalNamespaceValue = (MirObjectId, Span);

/// Stores all the identifiers that are local to a given object or context.
#[derive(Debug, Default, Clone)]
pub struct MirLocalNamespace {
    /// All properties of this namespace.
    properties: FxHashMap<Ident, LocalNamespaceValue>,
}

impl MirLocalNamespace {
    pub fn iter(&self) -> impl Iterator<Item = (&Ident, &LocalNamespaceValue)> {
        self.properties.iter()
    }

    pub fn get_property(&self, ident: &Ident) -> Option<MirObjectId> {
        self.properties.get(ident).map(|(obj_id, _)| *obj_id)
    }

    pub fn property_get_or_insert(
        &mut self,
        namespace: &mut MirNamespace,
        ident: Ident,
        span: Span,
        current_context_id: MirContextId,
    ) -> MirObjectId {
        self.properties
            .entry(ident.clone())
            .or_insert_with(|| {
                let obj_id = namespace.insert_object(current_context_id).id;
                namespace.add_maybe_erroneous_obj_info(obj_id, (ident, span));
                (obj_id, span)
            })
            .0
    }

    pub fn insert(&mut self, id: MirObjectId, ident: Ident, span: Span) {
        self.properties.insert(ident, (id, span));
    }
}
