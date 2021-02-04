use crate::{
    error::{LangError, LangErrorKind, Result},
    function_interface::DebrisFunctionInterface,
    llir::utils::ItemId,
    objects::{obj_class::GenericClassRef, obj_null::ObjNull},
    CompileContext, ObjectRef, TypePattern,
};
use debris_common::{Ident, Span};
use std::{fmt::Debug, rc::Rc};

use super::ContextId;

/// Any value that is used in the mir compilation and also in the llir
///
/// Marks either a concrete object or a placeholder for a concrete object
#[derive(Eq, PartialEq, Clone)]
pub enum MirValue {
    /// A concrete object
    Concrete(ObjectRef),
    /// A template which marks a future object
    ///
    /// id: A unique id for this template
    /// template: The type (or super class) of the object
    Template { id: ItemId, class: GenericClassRef },
}

/// A function call to api functions
pub struct MirCall {
    pub span: Span,
    pub function: Rc<DebrisFunctionInterface>,
    pub value: ObjectRef,
    pub parameters: Vec<MirValue>,
    pub return_value: MirValue,
}

/// Calls a specific context
#[derive(Debug, PartialEq, Eq)]
pub struct MirGotoContext {
    pub span: Span,
    pub context_id: ContextId,
}

/// Acts like `MirGotoContext`, if the condition is equal to true
#[derive(Debug, PartialEq, Eq)]
pub struct MirBranchIf {
    pub span: Span,
    pub pos_branch: ContextId,
    pub neg_branch: ContextId,
    /// The values returned by positive and negative branch
    pub pos_value: MirValue,
    pub neg_value: MirValue,
    /// The id of the item returned by the block
    /// None if nothing needs to be returned
    pub value_id: Option<ItemId>,
    /// The condition, has to be a boolean (right now)
    pub condition: MirValue,
}

/// Any node that can be part of the mir representation
#[derive(Debug, Eq, PartialEq)]
pub enum MirNode {
    Call(MirCall),
    GotoContext(MirGotoContext),
    BranchIf(MirBranchIf),
}

impl MirValue {
    /// Creates a new mir value that is null
    pub fn null(ctx: &CompileContext) -> Self {
        MirValue::Concrete(ObjNull::instance(ctx))
    }

    /// Gets a property from the value
    ///
    /// Called if it does not matter whether this is an actual value or a template,
    /// because a class attribute gets accessed
    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        match self {
            MirValue::Concrete(object_ref) => object_ref.get_property(ident),
            MirValue::Template { id: _, class } => class.get_property(ident),
        }
    }

    /// Returns the class of this value
    pub fn class(&self) -> &GenericClassRef {
        match self {
            MirValue::Concrete(obj) => &obj.class,
            MirValue::Template { id: _, class } => &class,
        }
    }

    pub fn expect_concrete(&self, message: &str) -> &ObjectRef {
        match self {
            MirValue::Concrete(concrete) => concrete,
            MirValue::Template { class, id } => panic!(
                "Expected a concrete mir_value, bot got template {} with id {:?}: {}",
                class, id, message
            ),
        }
    }

    pub fn concrete(&self) -> Option<ObjectRef> {
        match self {
            MirValue::Concrete(obj) => Some(obj.clone()),
            _ => None,
        }
    }

    pub fn expect_template(&self, message: &str) -> (GenericClassRef, ItemId) {
        match self {
            MirValue::Concrete(concrete) => panic!(
                "Expected a template mir_value, bot got object {} : {}",
                concrete.class.as_ref(),
                message
            ),
            MirValue::Template { class, id } => (class.clone(), *id),
        }
    }

    pub fn template(&self) -> Option<(GenericClassRef, ItemId)> {
        match self {
            MirValue::Template { id, class } => Some((class.clone(), *id)),
            _ => None,
        }
    }

    /// Asserts that the type of this value matches `class`
    /// and throws otherwise
    #[track_caller]
    pub fn assert_type(
        &self,
        typ: TypePattern,
        span: Span,
        declared_at: Option<Span>,
    ) -> Result<()> {
        let own_type = self.class();

        if !typ.matches(own_type) {
            Err(LangError::new(
                LangErrorKind::UnexpectedType {
                    got: self.class().clone(),
                    expected: typ,
                    declared: declared_at,
                },
                span,
            )
            .into())
        } else {
            Ok(())
        }
    }
}

impl From<ObjectRef> for MirValue {
    fn from(value: ObjectRef) -> Self {
        MirValue::Concrete(value)
    }
}

impl Debug for MirValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MirValue::Concrete(value) => f.write_fmt(format_args!("{:?}", value)),
            MirValue::Template { id, class } => f
                .debug_struct("TemplatedValue")
                .field("id", id)
                .field("class", &format_args!("{}", class))
                .finish(),
        }
    }
}

impl Debug for MirCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MirCall")
            .field("Span", &self.span)
            .field("value", &self.value)
            .field("parameters", &self.parameters)
            .field("return_value", &self.return_value)
            .finish()
    }
}

impl PartialEq for MirCall {
    fn eq(&self, other: &MirCall) -> bool {
        self.span == other.span
            && self.value == other.value
            && self.parameters == other.parameters
            && self.return_value == other.return_value
    }
}

impl Eq for MirCall {}
