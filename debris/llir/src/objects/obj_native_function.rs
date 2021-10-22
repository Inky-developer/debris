use core::fmt;

use crate::{impl_class, memory::MemoryLayout, NativeFunctionId, ObjectPayload, Type};

#[derive(Debug, PartialEq, Eq)]
pub struct ObjNativeFunction {
    pub function_id: NativeFunctionId,
}

impl_class! {ObjNativeFunction, Type::Function, {}}

impl ObjNativeFunction {}

impl fmt::Display for ObjNativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function({})", self.function_id)
    }
}

impl ObjectPayload for ObjNativeFunction {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}
