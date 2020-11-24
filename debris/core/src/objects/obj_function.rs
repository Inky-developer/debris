use std::fmt::{Debug, Display};

use debris_common::Span;
use debris_derive::object;

use crate::CompileContext;
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
type CallbackType = fn(&mut FunctionContext, &[ObjectRef]) -> LangResult<ObjectRef>;
// type CallbackType = dyn Fn(&mut FunctionContext, &[ObjectRef]) -> LangResult<ObjectRef>;

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

/// A signature describing a single overload of a function
#[derive(Eq, PartialEq)]
pub struct FunctionSignature {
    parameters: Vec<ClassRef>,
    return_type: ClassRef,
    callback_function: CallbackFunction,
}

impl FunctionSignature {
    pub fn new(
        parameters: Vec<ClassRef>,
        return_type: ClassRef,
        callback_function: CallbackFunction,
    ) -> Self {
        FunctionSignature {
            parameters,
            return_type,
            callback_function,
        }
    }

    /// Returns whether the args iterator matches all of the required arguments
    pub fn matches(&self, args: &[&ObjClass]) -> bool {
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

    pub fn function(&self) -> &CallbackFunction {
        &self.callback_function
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
