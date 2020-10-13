use std::{any::TypeId, ops::Deref};

use crate::{
    compile_context::TypeContext, CompileContext, DebrisObject, ObjectPayload, ObjectRef, Type,
};

use super::{ClassRef, ObjectClass};

/// A static string object
///
/// Very basic right now and supports no runtime functionality.
#[derive(Debug, Eq, PartialEq)]
pub struct ObjectString {
    value: String,
}

impl ObjectPayload for ObjectString {
    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new_ref(self.class(&ctx.type_ctx), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload()
            .map_or(false, |other| self == other)
    }
}

// #[template]
impl ObjectString {
    fn class(&self, ctx: &TypeContext) -> ClassRef {
        ctx.get_or_insert(TypeId::of::<Self>(), || {
            ObjectClass::new_empty(Type::String)
        })
    }
}

impl From<String> for ObjectString {
    fn from(value: String) -> Self {
        ObjectString { value }
    }
}

impl Deref for ObjectString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
