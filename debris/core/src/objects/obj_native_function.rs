use core::fmt;

use debris_derive::object;

use crate::{
    class::ClassRef,
    llir::{memory::MemoryLayout, utils::BlockId},
    ObjectPayload, ObjectRef, Type,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ObjNativeFunction {
    Function {
        block_id: BlockId,
        parameters: Vec<ObjectRef>,
        return_value: ObjectRef,
        return_type: ClassRef,
    },
}

#[object(Type::Function)]
impl ObjNativeFunction {}

impl fmt::Display for ObjNativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjNativeFunction::Function {
                block_id,
                parameters,
                return_value,
                return_type,
            } => write!(
                f,
                "fn <unknown>({:?}) -> {:?}: {:?} {{{:?}}}",
                parameters, return_value, return_type, block_id
            ),
        }
    }
}

impl ObjectPayload for ObjNativeFunction {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}
