use rustc_hash::FxHashMap;

use debris_common::{CompilationId, CompileContext, Ident};

use crate::mir_object::{MirObject, MirObjectId};

#[derive(Debug)]
pub struct MirNamespace {
    /// The objects are indexed by `MirObjectId.id`
    /// For this reason, no object may ever be removed from this vec
    objects: Vec<MirObject>,
    compilation_id: CompilationId,
}

impl MirNamespace {
    pub fn new(ctx: &CompileContext) -> Self {
        MirNamespace {
            objects: Vec::new(),
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
}

#[derive(Debug, Default, Clone)]
pub struct MirLocalNamespace {
    properties: FxHashMap<Ident, MirObjectId>,
}

impl MirLocalNamespace {
    pub fn iter(&self) -> impl Iterator<Item = (&Ident, &MirObjectId)> {
        self.properties.iter()
    }

    pub fn get_property(&self, ident: &Ident) -> Option<MirObjectId> {
        self.properties.get(ident).copied()
    }

    pub fn property_get_or_insert(
        &mut self,
        namespace: &mut MirNamespace,
        ident: Ident,
    ) -> MirObjectId {
        *self
            .properties
            .entry(ident)
            .or_insert_with(|| namespace.insert_object().id)
    }

    pub fn insert(&mut self, id: MirObjectId, ident: Ident) {
        self.properties.insert(ident, id);
    }
}
