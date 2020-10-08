use debris_derive::template;
use debris_type::Type;
use std::ops::Deref;

use crate::{CompileContext, DebrisObject, ObjectPayload, ObjectProperties, ObjectRef};

use super::{ObjectType, TypeRef};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjectString {
    value: String,
}

impl ObjectPayload for ObjectString {
    fn typ(&self) -> Type {
        Type::String
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload()
            .map_or(false, |other| self == other)
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new(ctx.type_ctx.template_for_type(&self.typ()), self)
    }
}

#[template]
impl ObjectString {
    pub fn template() -> TypeRef {
        ObjectType::new_ref(
            Type::Template(Box::new(Type::String)),
            ObjectProperties::default(),
            None,
        )
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
