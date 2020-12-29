use std::fmt::{Debug, Display};

use debris_derive::object;

use crate::{
    function_interface::DebrisFunctionInterface, llir::llir_nodes::Node, llir::utils::ItemId,
    CompileContext, ObjectPayload, ObjectRef, Type,
};

use super::{ClassRef, ObjClass};

/// A function object
///
/// Has a map of available signatures.
/// The call parameters are unique identifiers for every signature
pub struct ObjFunction {
    signatures: Vec<FunctionSignature>,
    /// A unique id for this function
    id: u64,
}

#[object(Type::Function)]
impl ObjFunction {
    pub fn new(ctx: &CompileContext, sig: impl Into<Vec<FunctionSignature>>) -> Self {
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
        ObjFunction {
            signatures,
            id: ctx.get_unique_id(),
        }
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

    /// Returns every possible signature as (params, return) which gets accepted by this function
    pub fn expected_signatures(&self) -> Vec<(FunctionParameters, ClassRef)> {
        self.signatures
            .iter()
            .map(|sig| (sig.parameters.clone(), sig.return_type.clone()))
            .collect()
    }
}

impl ObjectPayload for ObjFunction {}

impl PartialEq for ObjFunction {
    fn eq(&self, other: &ObjFunction) -> bool {
        self.id == other.id
    }
}

impl Eq for ObjFunction {}

impl Debug for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ObjectFunction").finish()
    }
}

/// Decides which arguments a function can accepts
#[derive(Debug, PartialEq, Eq, Clone)]
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

/// The context which gets passed to a function
pub struct FunctionContext<'a> {
    pub compile_context: &'a CompileContext,
    /// A vec of emitted nodes
    pub nodes: Vec<Node>,
    /// The id for the returned value
    pub item_id: ItemId,
}

/// A signature describing a single overload of a function
pub struct FunctionSignature {
    parameters: FunctionParameters,
    return_type: ClassRef,
    callback_function: DebrisFunctionInterface,
}

impl FunctionSignature {
    pub fn new(
        parameters: impl Into<FunctionParameters>,
        return_type: ClassRef,
        callback_function: DebrisFunctionInterface,
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

    pub fn function(&self) -> &DebrisFunctionInterface {
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

impl FunctionContext<'_> {
    /// Adds a node to the previously emitted nodes
    pub fn emit(&mut self, node: Node) {
        self.nodes.push(node);
    }

    /// Shortcut for returning `ObjNull`
    pub fn null(&self) -> ObjectRef {
        self.compile_context.type_ctx.null(&self.compile_context)
    }
}
