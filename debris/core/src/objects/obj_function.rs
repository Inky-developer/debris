use debris_common::Span;
use debris_type::Type;
use std::fmt::{Debug, Display};

use crate::{
    error::{LangError, LangResult, Result},
    llir::llir_nodes::Node,
    llir::utils::ItemId,
};
use crate::{CompileContext, DebrisObject};
use crate::{ObjectPayload, ObjectProperties, ObjectRef};

use super::{ObjectType, TypeRef};

/// A function object
///
/// Has a map of available signatures.
/// The call parameters are unique identifiers for every signature
#[derive(Eq, PartialEq)]
pub struct ObjectFunction {
    pub signatures: FunctionSignatureMap,
}

impl ObjectFunction {
    pub fn new<T: Into<FunctionSignatureMap>>(sig: T) -> Self {
        ObjectFunction {
            signatures: sig.into(),
        }
    }

    /// Creates a function without overload
    pub fn without_overload(
        parameters: Vec<Type>,
        return_type: Type,
        function: CallbackFunction,
    ) -> Self {
        ObjectFunction::new(FunctionSignatureMap::new(vec![(
            FunctionSignature::new(parameters, return_type),
            function,
        )]))
    }

    pub fn template() -> TypeRef {
        ObjectType::new_ref(
            Type::Template(Box::new(Type::Function)),
            ObjectProperties::default(),
            None,
        )
    }

    pub fn init_template(_: &CompileContext, _: &TypeRef) {}
}

impl ObjectPayload for ObjectFunction {
    fn typ(&self) -> Type {
        Type::Function
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new_ref(ctx.type_ctx.template_for_type(&self.typ()), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
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
type CallbackType = fn(&mut FunctionContext, Vec<ObjectRef>) -> LangResult<ObjectRef>;

/// Wrapper, so traits like `Eq` can be implemented
pub struct CallbackFunction(pub CallbackType);

impl CallbackFunction {
    pub fn call(
        &self,
        ctx: &CompileContext,
        span: &Span,
        parameters: Vec<ObjectRef>,
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
    parameters: Vec<Type>,
    return_type: Type,
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
        args: &[&Type],
    ) -> Option<&(FunctionSignature, CallbackFunction)> {
        self.signatures.iter().find(|(sig, _fun)| sig.matches(args))
    }
}

impl FunctionSignature {
    pub fn new(parameters: Vec<Type>, return_type: Type) -> Self {
        FunctionSignature {
            parameters,
            return_type,
        }
    }

    /// Returns whether the args iterator matches all of the required arguments
    ///
    /// # Examples
    /// ```
    /// use debris_type::Type;
    /// use debris_core::objects::FunctionSignature;
    ///
    /// let signature = FunctionSignature::new(
    ///     vec![Type::StaticInt, Type::StaticInt],
    ///     Type::StaticInt
    /// );
    /// assert!(signature.matches(&[&Type::StaticInt, &Type::StaticInt]));
    /// assert!(!signature.matches(&[&Type::StaticInt]));
    /// assert!(!signature.matches(&[&Type::StaticInt, &Type::StaticInt, &Type::StaticInt]));
    /// assert!(!signature.matches(&[&Type::DynamicInt, &Type::StaticInt]));
    /// ```
    pub fn matches(&self, args: &[&Type]) -> bool {
        self.parameters.len() == args.len()
            && self
                .parameters
                .iter()
                .zip(args.iter())
                .all(|(required, got)| required.matches(got))
    }

    pub fn parameters(&self) -> &[Type] {
        &self.parameters
    }

    pub fn return_type(&self) -> &Type {
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
            "fun ({:?}) -> {}",
            self.parameters
                .iter()
                .map(|typ| format!("{}", typ))
                .collect::<Vec<_>>()
                .join(", "),
            self.return_type
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
