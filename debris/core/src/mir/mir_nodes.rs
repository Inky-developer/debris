use crate::{
    llir::utils::ItemId,
    objects::{ClassRef, ObjNull},
    CompileContext, ObjectRef,
};
use debris_common::{Ident, LocalSpan};
use std::fmt::Debug;

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
    Template { id: u64, class: ClassRef },
}

/// Any node that can be part of the mir representation
#[derive(Debug, Eq, PartialEq)]
pub enum MirNode {
    /// A function call
    ///
    /// Can be a call to an ordinary function or
    /// a call to special functions like StaticInt.+
    Call {
        span: LocalSpan,
        value: ObjectRef,
        parameters: Vec<MirValue>,
        return_value: MirValue,
    },
    /// A context change
    ///
    /// Similar to a function call, but does not require arguments and
    /// always translated into minecraft function call (unless optimized out)
    GotoContext { span: LocalSpan, context_id: u64 },
    /// A raw command which will be evaluated into a string
    ///
    /// If compiling for a datapack this node will be 1:1 copied into the datapack
    RawCommand { value: MirValue, var_id: ItemId },
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
    pub fn class(&self) -> &ClassRef {
        match self {
            MirValue::Concrete(obj) => &obj.class,
            MirValue::Template { id: _, class } => &class,
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
                .field("class.type", &class.typ())
                .finish(),
        }
    }
}
