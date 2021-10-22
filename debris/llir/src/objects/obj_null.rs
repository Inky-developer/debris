use std::fmt;

use crate::{impl_class, memory::MemoryLayout, ObjectPayload, Type};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ObjNull;

impl_class! {ObjNull, Type::Null, {}}

impl ObjectPayload for ObjNull {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjNull {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Null")
    }
}

impl From<()> for ObjNull {
    fn from((): ()) -> Self {
        ObjNull
    }
}
