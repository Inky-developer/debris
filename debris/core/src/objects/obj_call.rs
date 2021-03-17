use std::{fmt, rc::Rc};

use crate::{
    error::LangResult,
    llir::llir_nodes::{Call, Node},
    memory::MemoryLayout,
    mir::{ContextId, MirValue},
    objects::obj_function::{FunctionOverload, FunctionParameters, FunctionSignature},
    CompileContext, ObjectPayload, ObjectRef, Type,
};
use debris_derive::object;

use super::obj_function::{FunctionContext, ObjFunction};

/// A call behaves much like a native function, however it
/// has no parameters and already a generated context.
/// This allows builtin functions to work with `ObjCall`.
#[derive(Debug, PartialEq, Eq)]
pub struct ObjCall {
    pub context_id: ContextId,
    function: ObjFunction,
}

#[object(Type::Function)]
impl ObjCall {
    pub fn new(ctx: &CompileContext, context_id: ContextId, return_value: MirValue) -> Self {
        let class = return_value.class().clone();
        let function = move |ctx: &mut FunctionContext, _: &[ObjectRef]| -> LangResult<ObjectRef> {
            let id = ctx.block_for(context_id);
            println!("Hi");
            ctx.emit(Node::Call(Call { id }));
            Ok(ctx.get_object(&return_value))
        };

        let debris_function = ObjFunction::new(
            ctx,
            vec![FunctionOverload::new(
                Rc::new(FunctionSignature::new(
                    FunctionParameters::Specific(vec![]),
                    class,
                )),
                function.into(),
            )],
        );

        ObjCall {
            context_id,
            function: debris_function,
        }
    }
}

impl ObjectPayload for ObjCall {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
    }

    fn as_function(&self) -> Option<&ObjFunction> {
        Some(&self.function)
    }
}

impl fmt::Display for ObjCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Call({})", self.context_id)
    }
}
