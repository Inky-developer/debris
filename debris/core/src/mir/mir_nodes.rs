use debris_common::{Ident, LocalSpan};
use debris_type::Type;

use std::fmt::Debug;

use crate::{llir::utils::ItemId, objects::TypeRef, ObjectRef};

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
    Template { id: u64, template: TypeRef },
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
    /// A raw command which will be evaluated into a string
    ///
    /// If compiling for a datapack this node will be 1:1 copied into the datapack
    RawCommand { value: MirValue, var_id: ItemId },
}

impl MirValue {
    /// Gets a property from the value
    ///
    /// Called if it does not matter whether this is an actual value or a template,
    /// because a class attribute gets accessed
    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        match self {
            MirValue::Concrete(object_ref) => object_ref.get_property(ident),
            MirValue::Template { id: _, template } => template.get_property(ident),
        }
    }

    /// The type of this value
    pub fn typ(&self) -> &Type {
        match self {
            MirValue::Concrete(object_ref) => &object_ref.typ,
            MirValue::Template { id: _, template } => match template.value_typ() {
                Type::Template(correct_type) => correct_type.as_ref(),
                other => other,
            },
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
            MirValue::Template { id, template } => f
                .debug_struct("TemplatedValue")
                .field("id", id)
                .field("template.type", &template.value_typ())
                .finish(),
        }
    }
}
