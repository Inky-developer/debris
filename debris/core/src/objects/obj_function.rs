use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use debris_common::{Ident, Span};
use debris_derive::object;
use generational_arena::Index;
use itertools::Itertools;

use crate::{
    function_interface::DebrisFunctionInterface,
    llir::llir_nodes::Node,
    llir::{llir_nodes::Function, utils::ItemId, LlirHelper},
    mir::{MirContext, MirNamespaceEntry, NamespaceArena},
    types::TypePattern,
    CompileContext, Namespace, ObjectPayload, ObjectRef, Type,
};

use super::{ClassRef, ObjClass};

/// A function object
///
/// Has a map of available signatures.
/// The call parameters are unique identifiers for every signature
pub struct ObjFunction {
    signatures: Vec<FunctionSignature>,
    /// A unique id for this function
    id: usize,
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
    pub fn expected_signatures(&self) -> Vec<(FunctionParameters, TypePattern)> {
        self.signatures
            .iter()
            .map(|sig| (sig.parameters.clone(), sig.return_type.clone().into()))
            .collect()
    }
}

impl ObjectPayload for ObjFunction {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ObjectFunction").finish()
    }
}

/// Decides which arguments a function can accepts
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionParameters {
    Any,
    Specific(Vec<TypePattern>),
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

/// The context which gets passed to a function
pub struct FunctionContext<'llir, 'ctx, 'ns> {
    pub compile_context: &'ctx CompileContext,
    pub namespaces: &'ns mut NamespaceArena,
    pub parent: Index,
    /// A vec of emitted nodes
    pub nodes: Vec<Node>,
    /// The id for the returned value
    pub item_id: ItemId,
    /// The current span
    pub span: Span,
    /// The previous mir contexts
    pub mir_contexts: &'ctx [MirContext<'ctx>],
    /// The functions that were already emmitted
    pub(crate) llir_helper: &'llir mut LlirHelper,
}

/// A signature describing a single overload of a function
pub struct FunctionSignature {
    parameters: FunctionParameters,
    /// This must be a class because the return type
    /// must be specified exactly for api function
    return_type: ClassRef,
    callback_function: Rc<DebrisFunctionInterface>,
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
            callback_function: Rc::new(callback_function),
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

impl FunctionContext<'_, '_, '_> {
    /// Adds a node to the previously emitted nodes
    pub fn emit(&mut self, node: Node) {
        self.nodes.push(node);
    }

    /// Creates a new lllir function
    pub fn add_function(&mut self, function: Function) {
        self.llir_helper.push(function);
    }

    /// Shortcut for returning `ObjNull`
    pub fn null(&self) -> ObjectRef {
        self.compile_context.type_ctx().null()
    }

    /// Creates a new namespace context which can be used to store local variables
    pub fn make_context(&mut self) -> Index {
        let parent = self.parent;
        self.namespaces
            .insert_with(|own| Namespace::new(own, Some(parent)))
    }

    /// Tries to get a property starting at the `start` namespace and searching down from there
    pub fn get_object(&self, start: Index, ident: &Ident) -> Option<ObjectRef> {
        self.namespaces
            .find_value(start, ident)
            .and_then(|value| value.concrete())
    }

    /// Sets an object in this namespace
    pub fn set_object(&mut self, namespace: Index, ident: Ident, value: ObjectRef) {
        let namespace = &mut self.namespaces[namespace];
        namespace.add_object(
            ident,
            MirNamespaceEntry::Spanned {
                span: self.span,
                value: value.into(),
            },
        );
    }
}
