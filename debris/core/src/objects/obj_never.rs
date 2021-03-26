use std::fmt;

use debris_derive::object;

use crate::{memory::MemoryLayout, CompileContext, ObjectPayload, ObjectRef, Type};

/// Signals that a block will never return.
/// The most interesting property of this type is its ability to match any other type.
/// This does not circumvent the entire type system, because any code that depends on
/// a value with the `Never` type must not be evaluated (in llir).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ObjNever;

#[object(Type::Never)]
impl ObjNever {
    pub fn instance(ctx: &CompileContext) -> ObjectRef {
        ctx.type_ctx().never()
    }
}

impl ObjectPayload for ObjNever {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjNever {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Never")
    }
}
