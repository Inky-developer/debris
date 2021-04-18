use std::fmt;

use debris_derive::object;

use super::obj_struct::StructRef;

use crate::{
    class::{Class, ClassKind, ClassRef},
    llir::utils::ItemId,
    memory::MemoryLayout,
    mir::{NamespaceArena, NamespaceIndex},
    CompileContext, Namespace, ObjectPayload, Type,
};

pub fn memory_ids(buf: &mut Vec<ItemId>, arena: &NamespaceArena, namespace: &Namespace) {
    for (class, id) in namespace.iter().filter_map(|(_, var)| var.template()) {
        class.kind.memory_ids(buf, arena, id);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjStructObject {
    pub struct_type: StructRef,
    pub variables: NamespaceIndex,
    memory_layout: MemoryLayout,
}

#[object(Type::StructObject)]
impl ObjStructObject {
    pub fn new(arena: &mut NamespaceArena, struct_type: StructRef, index: NamespaceIndex) -> Self {
        let namespace = arena.get(index);
        let mut buf = Vec::new();
        memory_ids(&mut buf, arena, namespace);
        let memory_layout = MemoryLayout::from(buf);

        ObjStructObject {
            memory_layout,
            struct_type,
            variables: index,
        }
    }
}

impl ObjectPayload for ObjStructObject {
    fn memory_layout(&self) -> &MemoryLayout {
        &self.memory_layout
    }

    fn create_class(&self, _: &CompileContext) -> ClassRef {
        let class_kind = ClassKind::StructObject {
            strukt: self.struct_type.clone(),
            namespace: self.variables,
        };
        let class = Class::new_empty(class_kind);
        ClassRef::new(class)
    }
}

impl fmt::Display for ObjStructObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{} {{ namespace: {} }}",
            self.struct_type.ident, self.variables
        ))
    }
}
