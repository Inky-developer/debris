use std::fmt::{Debug, Display};

use debris_common::Span;
use debris_derive::object;

use crate::{error::LangErrorKind, CompileContext};
use crate::{
    error::{LangError, LangResult, Result},
    llir::llir_nodes::Node,
    llir::utils::ItemId,
    Type,
};
use crate::{ObjectPayload, ObjectRef};

use super::{ClassRef, ObjClass};

/// A function object
///
/// Has a map of available signatures.
/// The call parameters are unique identifiers for every signature
#[derive(Eq, PartialEq)]
pub struct ObjFunction {
    signatures: Vec<FunctionSignature>,
}

#[object(Type::Function)]
impl ObjFunction {
    pub fn new(sig: impl Into<Vec<FunctionSignature>>) -> Self {
        /// Compares every signature to every other signature and returns false
        /// if two signatures have the same parameters
        fn compare_all(data: &[FunctionSignature]) -> bool {
            for (index, value) in data.iter().enumerate() {
                for other_value in data.iter().skip(index + 1) {
                    if value.parameters == other_value.parameters {
                        return false;
                    }
                }
            }

            true
        }

        let signatures: Vec<FunctionSignature> = sig.into();
        assert!(compare_all(&signatures), "Ambigous signatures detected");
        ObjFunction { signatures }
    }

    pub fn signature<'a>(
        &self,
        params: impl Iterator<Item = &'a ObjClass>,
    ) -> Option<&FunctionSignature> {
        let params = params.collect::<Vec<_>>();
        let mut signatures = self.signatures.iter().filter(|sig| sig.matches(&params));

        let first = signatures.next()?;
        // Should already be checked by ObjectFunction::new
        debug_assert!(
            signatures.next().is_none(),
            "Every signature has to have unique parameters"
        );

        Some(first)
    }
}

impl ObjectPayload for ObjFunction {}

impl Debug for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ObjectFunction").finish()
    }
}

/// The type of function which can be used as a callback
pub type CallbackType = fn(&mut FunctionContext, &[ObjectRef]) -> LangResult<ObjectRef>;
// type CallbackType = dyn Fn(&mut FunctionContext, &[ObjectRef]) -> LangResult<ObjectRef>;

/// Decides which arguments a function can accepts
#[derive(Debug, PartialEq, Eq)]
pub enum FunctionParameters {
    Any,
    Specific(Vec<ClassRef>),
}

impl FunctionParameters {
    fn matches(&self, parameters: &[&ObjClass]) -> bool {
        match self {
            FunctionParameters::Any => true,
            FunctionParameters::Specific(required) => {
                required.len() == parameters.len()
                    && required
                        .iter()
                        .zip(parameters.iter())
                        .all(|(required, got)| got.matches(required))
            }
        }
    }
}

impl From<Vec<ClassRef>> for FunctionParameters {
    fn from(parameters: Vec<ClassRef>) -> Self {
        FunctionParameters::Specific(parameters)
    }
}

/// Wrapper, so traits like `Eq` can be implemented
pub struct CallbackFunction(pub CallbackType);

impl CallbackFunction {
    pub fn call(
        &self,
        ctx: &CompileContext,
        span: Span,
        parameters: &[ObjectRef],
        return_id: ItemId,
    ) -> Result<(ObjectRef, Vec<Node>)> {
        let mut function_ctx = FunctionContext {
            compile_context: ctx,
            nodes: vec![],
            item_id: return_id,
        };

        (self.0)(&mut function_ctx, parameters)
            .map(|value| (value, function_ctx.nodes))
            .map_err(|kind| LangError::new(kind, span).into())
    }
}

/// The context which gets passed to a function
pub struct FunctionContext<'a> {
    pub compile_context: &'a CompileContext,
    /// A vec of emitted nodes
    pub nodes: Vec<Node>,
    /// The id for the returned value
    pub item_id: ItemId,
}

/// A signature describing a single overload of a function
#[derive(Eq, PartialEq)]
pub struct FunctionSignature {
    parameters: FunctionParameters,
    return_type: ClassRef,
    callback_function: CallbackFunction,
}

impl FunctionSignature {
    pub fn new(
        parameters: impl Into<FunctionParameters>,
        return_type: ClassRef,
        callback_function: CallbackFunction,
    ) -> Self {
        FunctionSignature {
            parameters: parameters.into(),
            return_type,
            callback_function,
        }
    }

    /// Returns whether the args iterator matches all of the required arguments
    pub fn matches(&self, args: &[&ObjClass]) -> bool {
        self.parameters.matches(args)
    }

    pub fn parameters(&self) -> &FunctionParameters {
        &self.parameters
    }

    pub fn return_type(&self) -> &ClassRef {
        &self.return_type
    }

    pub fn function(&self) -> &CallbackFunction {
        &self.callback_function
    }
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "fun ({}) -> {:?}",
            match &self.parameters {
                FunctionParameters::Any => "..{Any}".to_string(),
                FunctionParameters::Specific(parameters) => parameters
                    .iter()
                    .map(|typ| format!("{:?}", typ.typ()))
                    .collect::<Vec<_>>()
                    .join(", "),
            },
            self.return_type.typ()
        ))
    }
}

// I don't understand why this can't be derived
impl PartialEq for CallbackFunction {
    fn eq(&self, other: &CallbackFunction) -> bool {
        self.0 as usize == other.0 as usize
    }
}

impl Eq for CallbackFunction {}

impl From<CallbackType> for CallbackFunction {
    fn from(val: CallbackType) -> Self {
        CallbackFunction(val)
    }
}

impl FunctionContext<'_> {
    /// Adds a node to the previously emitted nodes
    pub fn emit(&mut self, node: Node) {
        self.nodes.push(node);
    }

    /// Shortcut for returning `ObjNull`
    pub fn null(&self) -> ObjectRef {
        self.compile_context.type_ctx.null(&self.compile_context)
    }

    /// converts the parameters into a tuple of downcasted objects or raises an error
    ///
    /// Example:
    ///
    /// ```ignore
    /// fn do_something(ctx: &mut FunctionContext, parameters: &[ObjectRef]) -> LangResult<ObjNull> {
    ///     let (param_1, param_2): (ObjInt, ObjString) = FunctionContext::expect_types(parameters);
    ///     // [...]
    ///     Ok(ctx.null())
    /// }
    /// ```
    pub fn expect_types<'a, T>(parameters: &'a [ObjectRef]) -> LangResult<T>
    where
        T: ObjectTuple<'a>,
    {
        T::from_array(parameters)
    }
}

/// Syntactic sugar to convert from a list of objects to
/// a tuple of heterogenous `impl ObjectPayload` values
///
/// At the moment only implemented for tuples of size 1 and 2, but
/// if this trait gets used it can be expanded using e.g.
/// [this](https://crates.io/crates/impl-trait-for-tuples) crate.
///
/// Also, it just panics if the parameters cannot be downcasted which has to be fixed
pub trait ObjectTuple<'a>
where
    Self: Sized,
{
    fn from_array(array: &'a [ObjectRef]) -> LangResult<Self>;
}

impl<'a, T1, T2> ObjectTuple<'a> for (&'a T1, &'a T2)
where
    T1: ObjectPayload,
    T2: ObjectPayload,
{
    fn from_array(array: &'a [ObjectRef]) -> LangResult<Self> {
        match array {
            [a, b] => Ok((a.downcast_payload().unwrap(), b.downcast_payload().unwrap())),
            other => Err(LangErrorKind::UnexpectedOverload {
                parameters: other.iter().map(|obj| obj.class.clone()).collect(),
            }),
        }
    }
}

impl<'a, T1> ObjectTuple<'a> for (&'a T1,)
where
    T1: ObjectPayload,
{
    fn from_array(array: &'a [ObjectRef]) -> LangResult<Self> {
        match array {
            [a] => Ok((a.downcast_payload().unwrap(),)),
            other => Err(LangErrorKind::UnexpectedOverload {
                parameters: other.iter().map(|obj| obj.class.clone()).collect(),
            }),
        }
    }
}
