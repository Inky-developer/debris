use core::fmt;

use debris_error::LangResult;

use crate::{
    impl_class,
    memory::MemoryLayout,
    objects::{obj_function::FunctionContext, obj_function_ref::ObjFunctionRef},
    NativeFunctionId, ObjectPayload, Type,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjNativeFunction {
    pub function_id: NativeFunctionId,
}

impl_class! {ObjNativeFunction, Type::Function, {
    "compile" => |ctx: &mut FunctionContext, this: &ObjNativeFunction| -> LangResult<ObjFunctionRef> {
        let block_id = ctx.compile_native_function(this.function_id)?;
        Ok(ObjFunctionRef::new(block_id))
    }
}}

impl ObjNativeFunction {}

impl fmt::Display for ObjNativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn({})", self.function_id)
    }
}

impl ObjectPayload for ObjNativeFunction {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}
