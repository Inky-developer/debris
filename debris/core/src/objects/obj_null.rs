use std::fmt;

use debris_derive::object;

use crate::{memory::MemoryLayout, CompileContext, ObjectPayload, ObjectRef, Type};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ObjNull;

#[object(Type::Null)]
impl ObjNull {
    pub fn instance(ctx: &CompileContext) -> ObjectRef {
        ctx.type_ctx().null()
    }
}

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
