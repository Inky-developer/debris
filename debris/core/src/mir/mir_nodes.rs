use crate::{
    error::{LangError, LangErrorKind, Result},
    function_interface::DebrisFunctionInterface,
    llir::utils::ItemId,
    objects::{obj_class::GenericClassRef, obj_null::ObjNull},
    CompileContext, ObjectRef, TypePattern,
};
use debris_common::{Ident, Span};
use itertools::Itertools;
use std::{
    fmt::{self, Debug},
    rc::Rc,
};

use super::ContextId;

/// Any value that is used in the mir compilation and also in the llir
///
/// Marks either a concrete object or a placeholder for a concrete object
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MirValue {
    /// A concrete object
    /// Since concrete objects are already known at the mir
    /// stage, and the mir is not responsible for checking
    /// the control flow, concrete objects must be constant in the mir.
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

/// Calls a specific block of a context
#[derive(Debug, PartialEq, Eq)]
pub struct MirGotoContext {
    pub span: Span,
    pub context_id: ContextId,
    /// The nth block of this context to go to
    pub block_id: usize,
}

/// Command to start a new Block withing this context
/// This allows jumps to this specific position
#[derive(Debug, PartialEq, Eq)]
pub struct MirJumpLocation {
    pub index: usize,
}

/// Overrides the old value in the namespace struct
/// with `new_value`.
#[derive(Debug, PartialEq, Eq)]
pub struct MirUpdateValue {
    pub id: ItemId,
    pub new_value: MirValue,
}

/// Sets the return value for a given context
#[derive(Debug, PartialEq, Eq)]
pub struct MirReturnValue {
    /// A reference to the value to be returned,
    /// See `ReturnValues` of `MirContext`
    pub return_index: usize,
    /// The context to set the return value for
    pub context_id: ContextId,
}

/// Acts like `MirGotoContext`, if the condition is equal to true
#[derive(Debug, PartialEq, Eq)]
pub struct MirBranchIf {
    pub span: Span,
    pub pos_branch: ContextId,
    pub neg_branch: ContextId,
    /// The condition, has to be a boolean (right now)
    pub condition: MirValue,
}

/// Any node that can be part of the mir representation
#[derive(Debug, Eq, PartialEq)]
pub enum MirNode {
    /// A function call which goes to a new block
    /// and initializes the parameters.
    /// How function calls work will probably change in the future.
    Call(MirCall),
    /// Goes to a specific jump location.
    GotoContext(MirGotoContext),
    /// Marks a jump location. A jump location can be seen as a
    /// 'checkpoint'.
    JumpLocation(MirJumpLocation),
    /// Updates the value of a variable.
    /// This is the only node which may write to variables.
    UpdateValue(MirUpdateValue),
    /// Sets the return value for this context.
    /// Note that this does not have to belong to a
    /// return-statement.
    ReturnValue(MirReturnValue),
    /// Branches if a condition evaluates to true
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

    /// Asserts that the type of this value matches `typ`
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

impl fmt::Display for MirNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_value(value: &MirValue) -> String {
            match value {
                MirValue::Concrete(obj) => format!("{}", obj),
                MirValue::Template { id, class } => format!("{}({})", id, class),
            }
        }

        match self {
            MirNode::BranchIf(branch) => f.write_fmt(format_args!(
                "\tif {}, {}, {}",
                fmt_value(&branch.condition),
                branch.pos_branch,
                branch.neg_branch,
            )),
            MirNode::Call(call) => f.write_fmt(format_args!(
                "\t{} := call {}, ({})",
                fmt_value(&call.return_value),
                call.value.class,
                call.parameters
                    .iter()
                    .map(|value| fmt_value(value))
                    .join(", ")
            )),
            MirNode::GotoContext(goto) => f.write_fmt(format_args!(
                "\tgoto {}, {}",
                goto.context_id, goto.block_id
            )),
            MirNode::JumpLocation(loc) => f.write_fmt(format_args!("\n.{}:", loc.index)),
            MirNode::ReturnValue(ret) => f.write_fmt(format_args!(
                "\tset_ret {}, {}",
                ret.context_id, ret.return_index
            )),
            MirNode::UpdateValue(update) => f.write_fmt(format_args!(
                "\tupdate {}, {}",
                update.id,
                fmt_value(&update.new_value)
            )),
        }
    }
}
