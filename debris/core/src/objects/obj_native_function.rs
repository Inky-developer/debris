use debris_common::Ident;
use debris_derive::object;
use itertools::Itertools;

use crate::{error::LangResult, llir::LLIRBuilder, CompileContext, ObjectPayload, ObjectRef, Type};

use super::{ClassRef, FunctionContext, FunctionSignature, ObjFunction};

#[derive(Debug)]
pub struct FunctionParameterDefinition {
    pub name: Ident,
    pub expected_type: ClassRef,
}

/// A callable function object for functions declared in debris
///
/// A function is converted from hir to mir exactly once
/// and from mir to llir every time the function gets called
#[derive(Debug, PartialEq, Eq)]
pub struct ObjNativeFunction {
    context_id: u64,
    function: ObjFunction,
}

#[object(Type::Function)]
impl ObjNativeFunction {
    pub fn new(
        ctx: &CompileContext,
        context_id: u64,
        parameters: Vec<FunctionParameterDefinition>,
        return_type: ClassRef,
    ) -> Self {
        let parameter_types: Vec<ClassRef> = parameters
            .iter()
            .map(|param| param.expected_type.clone())
            .collect();

        let function =
            move |ctx: &mut FunctionContext, objects: &[ObjectRef]| -> LangResult<ObjectRef> {
                let namespace = ctx.make_context();
                for (obj, param) in objects.iter().zip_eq(parameters.iter()) {
                    ctx.set_object(namespace, param.name.clone(), obj.clone());
                }

                let context = ctx
                    .mir_contexts
                    .iter()
                    .find(|ctx| ctx.id == context_id)
                    .expect("Context must exist");

                let llir_builder = LLIRBuilder::new(context, ctx.namespaces, ctx.mir_contexts);
                let mut function = llir_builder
                    .build()
                    .expect("ToDo make this error message compatible");

                ctx.nodes.append(&mut function.nodes);

                Ok(ctx.null())
            };

        let object_function = ObjFunction::new(
            ctx,
            vec![FunctionSignature::new(
                parameter_types,
                return_type,
                function.into(),
            )],
        );

        ObjNativeFunction {
            context_id,
            function: object_function,
        }
    }
}

impl ObjectPayload for ObjNativeFunction {
    fn as_function(&self) -> Option<&ObjFunction> {
        Some(&self.function)
    }
}
