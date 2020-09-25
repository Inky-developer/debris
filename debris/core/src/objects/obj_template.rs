use debris_common::Ident;
use debris_type::Type;
use std::any::Any;

use crate::{CompileContext, DebrisObject, ObjectPayload, ObjectProperties, ObjectRef};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjectTemplate {
    typ: Type,
    static_values: ObjectProperties,
}

impl ObjectTemplate {
    pub fn new(inner_type: Type, static_values: ObjectProperties) -> Self {
        ObjectTemplate {
            typ: Type::Template(Box::new(inner_type)),
            static_values,
        }
    }

    pub fn get_property(&self, ident: &Ident) -> Option<&ObjectRef> {
        self.static_values.get(ident)
    }

    pub fn get_type(&self) -> &Type {
        &self.typ
    }
}

impl ObjectPayload for ObjectTemplate {
    fn as_any(&self) -> &dyn Any {
        self as &dyn Any
    }

    fn typ(&self) -> Type {
        self.typ.clone()
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new(ctx.type_ctx.template_for_type(&self.typ), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
    }
}
