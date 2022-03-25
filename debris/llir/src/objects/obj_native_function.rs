use core::fmt;
use debris_error::LangResult;
use std::rc::Rc;

use crate::{
    class::{Class, ClassKind, ClassRef},
    impl_class,
    memory::MemoryLayout,
    objects::{
        obj_class::HasClass, obj_function::FunctionContext, obj_function_ref::ObjFunctionRef,
    },
    type_context::TypeContext,
    NativeFunctionId, ObjectPayload, Type,
};

use super::obj_function::FunctionClassRef;

#[derive(Debug, PartialEq, Eq)]
pub struct ObjNativeFunction {
    pub function_id: NativeFunctionId,
    pub(crate) signature: FunctionClassRef,
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
        fmt::Display::fmt(&self.signature, f)
    }
}

impl ObjectPayload for ObjNativeFunction {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn create_class(&self, ctx: &TypeContext) -> ClassRef {
        let kind = ClassKind::Function(Rc::clone(&self.signature));
        let class = Class {
            kind,
            properties: Self::create_properties(ctx).into(),
        };
        class.into()
    }
}
