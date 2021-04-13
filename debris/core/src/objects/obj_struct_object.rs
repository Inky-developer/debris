use std::fmt;

use debris_common::Ident;
use debris_derive::object;
use generational_arena::Index;

use super::obj_struct::StructRef;

use crate::{
    class::{Class, ClassKind, ClassRef},
    memory::MemoryLayout,
    mir::NamespaceArena,
    CompileContext, ObjectPayload, ObjectRef, Type,
};

#[derive(Debug, PartialEq, Eq)]
pub struct ObjStructObject {
    pub struct_type: StructRef,
    pub variables: Index,
    memory_layout: MemoryLayout,
}

#[object(Type::StructObject)]
impl ObjStructObject {
    pub fn new(arena: &mut NamespaceArena, struct_type: StructRef, index: Index) -> Self {
        let namespace = &arena[index];
        let memory_layout = MemoryLayout::Multiple(
            namespace
                .iter()
                .map(|(_, var)| var.expect_template("All ids must be known here").1)
                .collect(),
        );

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

    fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.struct_type.properties.get(ident).cloned()
    }

    fn create_class(&self, _: &CompileContext) -> ClassRef {
        let class_kind = ClassKind::Struct(self.struct_type.clone());
        let class = Class::new_empty(class_kind);
        ClassRef::new(class)
    }
}

impl fmt::Display for ObjStructObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{} {{ namespace: {} }}",
            self.struct_type.ident,
            self.variables.into_raw_parts().0
        ))
    }
}
