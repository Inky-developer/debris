use debris_common::Span;
use debris_derive::{object, ObjectPayload};
use std::fmt::{Debug, Display};

use crate::CompileContext;
use crate::ObjectRef;
use crate::{
    error::{LangError, LangResult, Result},
    llir::llir_nodes::Node,
    llir::utils::ItemId,
    Type,
};

use super::{ClassRef, ObjectClass};

/// A function object
///
/// Has a map of available signatures.
/// The call parameters are unique identifiers for every signature
#[derive(Eq, PartialEq, ObjectPayload)]
pub struct ObjectFunction {
    pub signatures: FunctionSignatureMap,
}

#[object(Type::Function)]
impl ObjectFunction {
    pub fn new(sig: impl Into<FunctionSignatureMap>) -> Self {
        ObjectFunction {
            signatures: sig.into(),
        }
    }

    /// Creates a function without overload
    pub fn without_overload(
        parameters: Vec<ClassRef>,
        return_type: ClassRef,
        function: CallbackFunction,
    ) -> Self {
        ObjectFunction::new(FunctionSignatureMap::new(vec![(
            FunctionSignature::new(parameters, return_type),
            function,
        )]))
    }
}

impl Debug for ObjectFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ObjectFunction")
            .field(&self.signatures)
            .finish()
    }
}

/// The type of function which can be used as a callback
type CallbackType = fn(&mut FunctionContext, &[ObjectRef]) -> LangResult<ObjectRef>;

/// Wrapper, so traits like `Eq` can be implemented
pub struct CallbackFunction(pub CallbackType);

impl CallbackFunction {
    pub fn call(
        &self,
        ctx: &CompileContext,
        span: &Span,
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
            .map_err(|kind| LangError::new(kind, span.clone()).into())
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

/// A collection of overloads for the same function
///
/// As of right now, I only want to allow overloads for special functions (binary operations), but this can support more general functions as well
#[derive(Eq, PartialEq)]
pub struct FunctionSignatureMap {
    signatures: Vec<(FunctionSignature, CallbackFunction)>,
}

/// A signature describing a single overload of a function
#[derive(Debug, Eq, PartialEq)]
pub struct FunctionSignature {
    parameters: Vec<ClassRef>,
    return_type: ClassRef,
}

impl FunctionSignatureMap {
    pub fn new(signatures: Vec<(FunctionSignature, CallbackFunction)>) -> Self {
        FunctionSignatureMap { signatures }
    }
    /// Finds the function that matches all of the args
    ///
    /// Returns None if no matching signature could be found
    pub fn function_for_args(
        &self,
        args: &[&ObjectClass],
    ) -> Option<&(FunctionSignature, CallbackFunction)> {
        self.signatures.iter().find(|(sig, _fun)| sig.matches(args))
    }
}

impl FunctionSignature {
    pub fn new(parameters: Vec<ClassRef>, return_type: ClassRef) -> Self {
        FunctionSignature {
            parameters,
            return_type,
        }
    }

    /// Returns whether the args iterator matches all of the required arguments
    pub fn matches(&self, args: &[&ObjectClass]) -> bool {
        self.parameters.len() == args.len()
            && self
                .parameters
                .iter()
                .zip(args.iter())
                .all(|(required, got)| required.matches(got))
    }

    pub fn parameters(&self) -> &[ClassRef] {
        &self.parameters
    }

    pub fn return_type(&self) -> &ClassRef {
        &self.return_type
    }
}

impl Debug for FunctionSignatureMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("FunctionSignatureMap")
            .field(&format_args!(
                "{}",
                self.signatures
                    .iter()
                    .map(|(sig, _fun)| format!("{}", sig))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
            .finish()
    }
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "fun ({}) -> {:?}",
            self.parameters
                .iter()
                .map(|typ| format!("{:?}", typ.typ()))
                .collect::<Vec<_>>()
                .join(", "),
            self.return_type.typ()
        ))
    }
}

impl FunctionContext<'_> {
    /// Adds a node to the previously emitted nodes
    pub fn emit(&mut self, node: Node) {
        self.nodes.push(node);
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
