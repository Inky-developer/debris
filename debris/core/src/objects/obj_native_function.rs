use std::{fmt, rc::Rc};

use debris_common::{Ident, Span};
use debris_derive::object;
use itertools::{EitherOrBoth, Itertools};

use crate::{
    error::LangResult,
    memory::MemoryLayout,
    mir::{CachedFunctionSignature, ContextId},
    types::TypePattern,
    CompileContext, ObjectPayload, ObjectRef, Type,
};

use super::{
    obj_class::{GenericClass, GenericClassRef, HasClass},
    obj_function::{
        FunctionContext, FunctionOverload, FunctionParameters, FunctionSignature, ObjFunction,
    },
};

pub fn match_parameters<'a>(
    signature: &[FunctionParameterDefinition],
    call: impl Iterator<Item = &'a GenericClass>,
) -> bool {
    for value in signature.iter().zip_longest(call) {
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjNativeFunction {
    context_id: ContextId,
    function: ObjFunction,
}

#[object(Type::Function)]
impl ObjNativeFunction {
    pub fn new(
        ctx: &CompileContext,
        context_id: ContextId,
        signature: Rc<CachedFunctionSignature>,
        return_type: GenericClassRef,
    ) -> Self {
        let parameters = FunctionParameters::Specific(
            signature
                .parameters
                .iter()
                .map(|value| value.expected_type.clone())
                .collect(),
        );
        let function = move |ctx: &mut FunctionContext, _: &[ObjectRef]| -> LangResult<ObjectRef> {
            // The arguments may be ignored here, because the native function already
            // gets evaluated in the mir for every call, thus all parameters are known
            ctx.call(context_id)
        };

        let object_function = ObjFunction::new(
            ctx,
            vec![FunctionOverload::new(
                FunctionSignature::new(parameters, return_type).into(),
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
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
    }

    fn as_function(&self) -> Option<&ObjFunction> {
        Some(&self.function)
    }
}

impl fmt::Display for ObjNativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NativeFunction")
    }
}

/// Created when the mir comes across a function definition, no actual function gets created
/// The Native Function objects get created for each call to such a function signature
#[derive(Debug, Clone)]
pub struct ObjNativeFunctionSignature {
    pub native_function_id: usize,
    pub function_span: Span,
    pub return_type_span: Span,
    pub definition_scope: ContextId,

    /// The generic class which contains the in and out parameters
    generic_class: GenericClassRef,
}

#[object(Type::Function)]
impl ObjNativeFunctionSignature {
    // Maybe I'll fix that later
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        ctx: &CompileContext,
        native_function_id: usize,
        function_span: Span,
        return_type_span: Span,
        definition_scope: ContextId,
        parameters: &[FunctionParameterDefinition],
        return_type: TypePattern,
    ) -> Self {
        let mut class = GenericClass::new(&Self::class(ctx));
        class.set_generics(
            "In".to_string(),
            parameters.iter().map(|p| p.expected_type.clone()).collect(),
        );
        class.set_generics("Out".to_string(), vec![return_type]);
        let generic_class = class.into_class_ref();
        ObjNativeFunctionSignature {
            native_function_id,
            function_span,
            return_type_span,
            definition_scope,
            generic_class,
        }
    }
}

impl ObjectPayload for ObjNativeFunctionSignature {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
    }

    fn generic_class(&self, _: &CompileContext) -> GenericClassRef {
        self.generic_class.clone()
    }
}

impl fmt::Display for ObjNativeFunctionSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Sig {}", self.generic_class)
    }
}

impl PartialEq for ObjNativeFunctionSignature {
    fn eq(&self, other: &ObjNativeFunctionSignature) -> bool {
        self.native_function_id == other.native_function_id
    }
}

impl Eq for ObjNativeFunctionSignature {}
