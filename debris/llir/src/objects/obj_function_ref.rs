use core::fmt;

use crate::{
    block_id::BlockId, impl_class, json_format::JsonFormatComponent, memory::MemoryLayout,
    ObjectPayload, Type,
};

/// A reference to an already generated native function
/// A function ref is uniquely identified by its block id
/// and (for now) has no parameters
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjFunctionRef {
    pub block_id: BlockId,
}

impl_class! {ObjFunctionRef, Type::FunctionRef, {} }

impl ObjFunctionRef {
    pub fn new(block_id: BlockId) -> Self {
        ObjFunctionRef { block_id }
    }
}

impl fmt::Display for ObjFunctionRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn ref({})", self.block_id)
    }
}

impl ObjectPayload for ObjFunctionRef {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn json_fmt(&self, buf: &mut Vec<JsonFormatComponent>) {
        buf.push(JsonFormatComponent::Function(self.block_id));
    }
}
