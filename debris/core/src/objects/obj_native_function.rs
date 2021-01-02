use debris_common::{Ident, Span};
use debris_derive::object;
use generational_arena::Index;
use itertools::{EitherOrBoth, Itertools};

use crate::{
    error::LangResult,
    hir::hir_nodes::HirBlock,
    llir::{
        llir_nodes::{Call, Node},
        LLIRBuilder,
    },
    types::TypePattern,
    CompileContext, ObjectPayload, ObjectRef, Type,
};

use super::{
    FunctionContext, FunctionSignature, GenericClass, GenericClassRef, HasClass, ObjFunction,
};

#[derive(Debug, Clone)]
pub struct FunctionParameterDefinition {
    pub name: Ident,
    pub expected_type: TypePattern,
    pub span: Span,
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
        return_type: GenericClassRef,
    ) -> Self {
        let parameter_types: Vec<TypePattern> = parameters
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

                let llir_builder =
                    LLIRBuilder::new(context, ctx.namespaces, ctx.mir_contexts, ctx.llir_helper);
                let id = llir_builder.function_id;
                let return_value = llir_builder
                    .build()
                    .expect("ToDo make this error message compatible");

                // and finally call this function
                ctx.emit(Node::Call(Call { id }));

                Ok(return_value)
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

/// Created when the mir comes across a function definition, no actual function gets created
/// The Native Function objects get created for each call to such a function signature
#[derive(Debug)]
pub struct ObjNativeFunctionSignature {
    pub native_function_id: usize,
    pub block: HirBlock,
    pub parameter_signature: Vec<FunctionParameterDefinition>,
    pub return_type: TypePattern,
    pub return_type_span: Span,
    pub definition_scope: Index,

    /// The generic class which contains the in and out parameters
    generic_class: GenericClassRef,
}

#[object(Type::Function)]
impl ObjNativeFunctionSignature {
    pub fn new(
        ctx: &CompileContext,
        native_function_id: usize,
        block: HirBlock,
        parameter_signature: Vec<FunctionParameterDefinition>,
        return_type: TypePattern,
        return_type_span: Span,
        definition_scope: Index,
    ) -> Self {
        let mut class: GenericClass = Self::class(ctx).into();
        class.set_generics(
            "In".to_string(),
            parameter_signature
                .iter()
                .map(|p| p.expected_type.clone())
                .collect(),
        );
        class.set_generics("Out".to_string(), vec![return_type.clone()]);
        let generic_class = class.into_class_ref();
        ObjNativeFunctionSignature {
            native_function_id,
            block,
            parameter_signature,
            return_type,
            return_type_span,
            definition_scope,
            generic_class,
        }
    }

    pub fn match_parameters<'a>(&self, call: impl Iterator<Item = &'a GenericClass>) -> bool {
        for value in self.parameter_signature.iter().zip_longest(call) {
            match value {
                EitherOrBoth::Both(expected, got) => {
                    if !expected.expected_type.matches(got) {
                        return false;
                    }
                }
                _ => return false,
            }
        }

        true
    }
}

impl ObjectPayload for ObjNativeFunctionSignature {
    fn generic_class(&self, _: &CompileContext) -> GenericClassRef {
        self.generic_class.clone()
    }
}

impl PartialEq for ObjNativeFunctionSignature {
    fn eq(&self, other: &ObjNativeFunctionSignature) -> bool {
        self.native_function_id == other.native_function_id
    }
}

impl Eq for ObjNativeFunctionSignature {}
