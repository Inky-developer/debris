use std::{
    fmt::{self, Debug, Display},
    rc::Rc,
};

use debris_common::{Ident, Span};
use debris_derive::object;

use itertools::Itertools;

use crate::{
    error::{CompileError, LangResult},
    function_interface::{DebrisFunctionInterface, ToFunctionInterface, ValidReturnType},
    llir::{
        llir_nodes::{Call, Node},
        utils::{BlockId, ItemId},
        LlirBuilder,
    },
    memory::MemoryLayout,
    mir::{ContextId, MirValue},
    types::TypePattern,
    CompileContext, ObjectPayload, ObjectRef, Type,
};

use super::obj_class::{GenericClass, GenericClassRef};

/// A function object
///
/// Has a map of available signatures.
/// The call parameters are unique identifiers for every signature
#[derive(Clone)]
pub struct ObjFunction {
    overloads: Vec<FunctionOverload>,
    /// A unique id for this function
    id: usize,
    pub flags: FunctionFlags,
}

#[object(Type::Function)]
impl ObjFunction {
    pub fn new(ctx: &CompileContext, overloads: Vec<FunctionOverload>) -> Self {
        Self::with_flags(ctx, overloads, FunctionFlags::default())
    }

    pub fn with_flags(
        ctx: &CompileContext,
        overloads: Vec<FunctionOverload>,
        flags: FunctionFlags,
    ) -> Self {
        /// Compares every signature to every other signature and returns false
        /// if two signatures have the same parameters
        fn compare_all(data: &[FunctionOverload]) -> bool {
            for (index, value) in data.iter().enumerate() {
                for other_value in data.iter().skip(index + 1) {
                    if value.signature().parameters == other_value.signature().parameters {
                        return false;
                    }
                }
            }

            true
        }

        assert!(compare_all(&overloads), "Ambigous signatures detected");
        ObjFunction {
            overloads,
            id: ctx.get_unique_id(),
            flags,
        }
    }

    pub fn new_single<T, Params, Return>(ctx: &CompileContext, function: &'static T) -> Self
    where
        T: ToFunctionInterface<Params, Return> + 'static,
        Return: ValidReturnType,
    {
        ObjFunction::new(
            ctx,
            vec![FunctionOverload::new(
                FunctionSignature::new(
                    T::query_parameters(ctx),
                    T::query_return(ctx).expect("This method must have a valid return type"),
                )
                .into(),
                function.to_function_interface().into(),
            )],
        )
    }

    pub fn overload<'a>(
        &self,
        params: impl Iterator<Item = &'a GenericClass>,
    ) -> Option<&FunctionOverload> {
        let params = params.collect::<Vec<_>>();
        let mut overloads = self
            .overloads
            .iter()
            .filter(|overload| overload.signature().matches(&params));

        let first = overloads.next()?;
        // Should already be checked by ObjectFunction::new
        debug_assert!(
            overloads.next().is_none(),
            "Every signature has to have unique parameters"
        );

        Some(first)
    }

    /// Returns every possible signature as (params, return) which gets accepted by this function
    pub fn expected_signatures(&self) -> impl Iterator<Item = Rc<FunctionSignature>> + '_ {
        self.overloads
            .iter()
            .map(|overload| overload.signature.clone())
    }
}

impl ObjectPayload for ObjFunction {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn as_function(&self) -> Option<&ObjFunction> {
        Some(self)
    }
}

impl PartialEq for ObjFunction {
    fn eq(&self, other: &ObjFunction) -> bool {
        self.id == other.id
    }
}

impl Eq for ObjFunction {}

impl Debug for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ObjectFunction").field(&self.id).finish()
    }
}

impl fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BuiltinFunction")
    }
}

/// Decides which arguments a function can accepts
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionParameters {
    Any,
    Specific(Vec<TypePattern>),
}

impl FunctionParameters {
    fn matches(&self, parameters: &[&GenericClass]) -> bool {
        match self {
            FunctionParameters::Any => true,
            FunctionParameters::Specific(required) => {
                required.len() == parameters.len()
                    && required
                        .iter()
                        .zip(parameters.iter())
                        .all(|(required, got)| required.matches(got))
            }
        }
    }
}

impl From<Vec<TypePattern>> for FunctionParameters {
    fn from(parameters: Vec<TypePattern>) -> Self {
        FunctionParameters::Specific(parameters)
    }
}

/// Flags a function can have. These effect how the compiler handles a function call.
#[derive(Debug, Clone, Copy)]
pub enum FunctionFlags {
    /// Marks that the function has no special flags
    None,
    /// Marks that the function should be implemented by the compiler
    CompilerImplemented(CompilerFunction),
}

impl Default for FunctionFlags {
    fn default() -> Self {
        FunctionFlags::None
    }
}

/// An enum over all functions which are implemented by the compiler
#[derive(Debug, Clone, Copy)]
pub enum CompilerFunction {
    RegisterTickingFunction,
}

/// The context which gets passed to a function
pub struct FunctionContext<'a, 'llir, 'ctx, 'arena> {
    /// The id for the returned value
    pub item_id: ItemId,
    /// The current span
    pub span: Span,
    pub llir_builder: &'a mut LlirBuilder<'llir, 'ctx, 'arena>,
}

pub type FunctionSignatureRef = Rc<FunctionSignature>;

/// A signature containing expected parameters and return type
#[derive(Debug)]
pub struct FunctionSignature {
    parameters: FunctionParameters,
    return_type: GenericClassRef,
}

impl FunctionSignature {
    pub fn new(parameters: FunctionParameters, return_type: GenericClassRef) -> Self {
        FunctionSignature {
            parameters,
            return_type,
        }
    }

    /// Returns whether the args iterator matches all of the required arguments
    pub fn matches(&self, args: &[&GenericClass]) -> bool {
        self.parameters.matches(args)
    }

    pub fn parameters(&self) -> &FunctionParameters {
        &self.parameters
    }

    pub fn return_type(&self) -> &GenericClassRef {
        &self.return_type
    }
}

/// A signature describing a single overload of a function
#[derive(Clone)]
pub struct FunctionOverload {
    signature: FunctionSignatureRef,
    callback_function: Rc<DebrisFunctionInterface>,
}

impl FunctionOverload {
    pub fn new(
        signature: FunctionSignatureRef,
        callback_function: DebrisFunctionInterface,
    ) -> Self {
        FunctionOverload {
            signature,
            callback_function: Rc::new(callback_function),
        }
    }

    pub fn signature(&self) -> &FunctionSignature {
        &self.signature
    }

    pub fn function(&self) -> Rc<DebrisFunctionInterface> {
        self.callback_function.clone()
    }
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "fun ({}) -> {}",
            match &self.parameters {
                FunctionParameters::Any => "..{Any}".to_string(),
                FunctionParameters::Specific(parameters) =>
                    parameters.iter().map(|typ| format!("{:?}", typ)).join(", "),
            },
            self.return_type
        ))
    }
}

impl FunctionContext<'_, '_, '_, '_> {
    pub fn compile_context(&self) -> &CompileContext {
        self.llir_builder.context.compile_context
    }

    /// Adds a node to the previously emitted nodes
    pub fn emit(&mut self, node: Node) {
        self.llir_builder.nodes.push(node);
    }

    /// Shortcut for returning `ObjNull`
    pub fn null(&self) -> ObjectRef {
        self.compile_context().type_ctx().null()
    }

    pub fn block_for(&mut self, context_id: ContextId) -> BlockId {
        self.llir_builder.llir_helper.block_for((context_id, 0))
    }

    pub fn get_object(&self, value: &MirValue) -> ObjectRef {
        self.llir_builder.get_object(value)
    }

    /// Tries to get a property starting at the `start` namespace and searching down from there
    pub fn get_object_by_ident(&self, start: ContextId, ident: &Ident) -> Option<ObjectRef> {
        self.llir_builder
            .arena
            .find_value(start, ident)
            .and_then(|value| value.concrete())
    }

    pub fn call(&mut self, context_id: ContextId) -> LangResult<ObjectRef> {
        let context = self.llir_builder.mir_contexts.get(context_id);

        let builder = LlirBuilder::new(
            context,
            self.llir_builder.arena,
            self.llir_builder.mir_contexts,
            self.llir_builder.llir_helper,
        );

        let id = builder.llir_helper.block_for((context_id, 0));

        let result = match builder.build() {
            Ok(result) => result,
            Err(e) => {
                return Err(match e {
                    CompileError::LangError(e) => e.kind,
                    CompileError::ParseError(_) => unreachable!(),
                })
            }
        };

        self.emit(Node::Call(Call { id }));

        Ok(result)
    }
}
