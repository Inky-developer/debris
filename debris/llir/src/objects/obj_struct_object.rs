use std::fmt;

use crate::{
    class::{Class, ClassKind, ClassRef},
    impl_class,
    memory::MemoryLayout,
    type_context::TypeContext,
    ObjectPayload, Type,
};

use super::obj_struct::StructRef;

#[derive(Debug, PartialEq, Eq)]
pub struct ObjStructObject {
    pub struct_type: StructRef,
    memory_layout: MemoryLayout,
}

impl_class! {ObjStructObject, Type::StructObject, {}}

impl ObjStructObject {
    pub fn new(_struct_type: StructRef) -> Self {
        todo!()
    }
}

impl ObjectPayload for ObjStructObject {
    fn memory_layout(&self) -> &MemoryLayout {
        &self.memory_layout
    }

    fn create_class(&self, _: &TypeContext) -> ClassRef {
        let class_kind = ClassKind::StructObject {
            strukt: self.struct_type.clone(),
        };
        let class = Class::new_empty(class_kind);
        ClassRef::new(class)
    }
}

impl fmt::Display for ObjStructObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{} {{Todo}}", self.struct_type.ident))
    }
}
