use debris_type::Type;
use std::any::Any;
use std::rc::Rc;

use crate::{objects::ObjectTemplate, CompileContext, DebrisObject};
use crate::{ObjectPayload, ObjectProperties, ObjectRef};

pub type TypeRef = Rc<ObjectType>;

#[derive(Debug, Eq, PartialEq)]
pub struct ObjectType {
    typ: Type,
}

pub fn type_template() -> ObjectTemplate {
    ObjectTemplate::new(Type::Type, ObjectProperties::default())
}

impl ObjectPayload for ObjectType {
    fn as_any(&self) -> &dyn Any {
        self as &dyn Any
    }

    fn typ(&self) -> Type {
        Type::Type
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new(ctx.type_ctx.template_for_type(&self.typ()), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
    }
}
