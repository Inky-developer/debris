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
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
    }
}

impl From<()> for ObjNull {
    fn from((): ()) -> Self {
        ObjNull
    }
}
