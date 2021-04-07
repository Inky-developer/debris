use std::fmt;

use debris_common::Ident;
use debris_derive::object;
use itertools::Itertools;

use super::obj_struct::ObjStruct;

use crate::{memory::MemoryLayout, Namespace, ObjectPayload, ObjectRef, Type};

#[derive(Debug, PartialEq, Eq)]
pub struct ObjStructObject {
    struct_type: ObjectRef,
    variables: Namespace,
    memory_layout: MemoryLayout,
}

#[object(Type::StructObject)]
impl ObjStructObject {
    pub fn new(struct_type: ObjectRef, variables: Namespace) -> Self {
        assert_eq!(struct_type.class.typ(), Type::Struct);

        let memory_layout = MemoryLayout::Multiple(
            variables
                .iter()
                .map(|(_, var)| var.expect_template("All ids must be known here").1)
                .collect(),
        );

        ObjStructObject {
            memory_layout,
            struct_type,
            variables,
        }
    }
    pub fn struct_type(&self) -> &ObjStruct {
        self.struct_type.downcast_payload::<ObjStruct>().unwrap()
    }
}

impl ObjectPayload for ObjStructObject {
    fn memory_layout(&self) -> &MemoryLayout {
        &self.memory_layout
    }

    fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.struct_type.get_property(ident)
    }
}

impl fmt::Display for ObjStructObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{} {{ {} }}",
            &self.struct_type().ident,
            self.variables
                .iter()
                .map(|(ident, var)| format!("{}: {}", ident, var.class()))
                .join(", ")
        ))
    }
}
