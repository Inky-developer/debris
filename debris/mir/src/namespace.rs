use rustc_hash::FxHashMap;

use debris_common::{CompilationId, CompileContext, Ident, Span};

use crate::mir_object::{MirObject, MirObjectId};

#[derive(Debug)]
pub struct MirNamespace {
    /// The objects are indexed by `MirObjectId.id`
    /// For this reason, no object may ever be removed from this vec
    objects: Vec<MirObject>,
    compilation_id: CompilationId,
    local_namespaces: Vec<MirLocalNamespace>,
}

impl MirNamespace {
    pub fn new(ctx: &CompileContext) -> Self {
        MirNamespace {
            objects: Vec::new(),
            local_namespaces: Vec::new(),
            compilation_id: ctx.compilation_id,
        }
    }

    pub fn insert_object(&mut self) -> &mut MirObject {
        let id = MirObjectId::new(self.compilation_id, self.objects.len() as u32);
        let object = MirObject::new(id);
        self.objects.push(object);
        self.objects.last_mut().unwrap()
    }

    pub fn get_obj_mut(&mut self, obj: MirObjectId) -> &mut MirObject {
        &mut self.objects[obj.id as usize]
    }

    pub fn get_obj(&self, obj: MirObjectId) -> &MirObject {
        &self.objects[obj.id as usize]
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
    ) -> MirObjectId {
        self.properties
            .entry(ident)
            .or_insert_with(|| (namespace.insert_object().id, span))
            .0
    }

    pub fn insert(&mut self, id: MirObjectId, ident: Ident, span: Span) {
        self.properties.insert(ident, (id, span));
    }
}
