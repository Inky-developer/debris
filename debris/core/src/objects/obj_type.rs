use debris_common::Ident;
use debris_derive::template;
use debris_type::Type;
use std::{cell::RefCell, rc::Rc};

use crate::{CompileContext, DebrisObject};
use crate::{ObjectPayload, ObjectProperties, ObjectRef};

pub type TypeRef = Rc<ObjectType>;

#[derive(Debug, Eq, PartialEq)]
pub struct ObjectType {
    typ: Type,
    properties: RefCell<ObjectProperties>,
    template: Option<TypeRef>,
}

#[template]
impl ObjectType {
    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.properties
            .borrow()
            .get(ident)
            .cloned()
            .or_else(|| match &self.template {
                Some(template) => template.get_property(ident),
                None => None,
            })
    }

    pub fn set_property(&self, ident: Ident, value: ObjectRef) {
        self.properties.borrow_mut().insert(ident, value);
    }

    pub fn new_ref(
        typ: Type,
        static_properties: ObjectProperties,
        template: Option<TypeRef>,
    ) -> TypeRef {
        Rc::new(ObjectType {
            typ,
            properties: RefCell::new(static_properties),
            template,
        })
    }

    pub fn template() -> TypeRef {
        ObjectType::new_ref(Type::Type, ObjectProperties::default(), None)
    }

    pub fn value_typ(&self) -> &Type {
        &self.typ
    }
}

impl ObjectPayload for ObjectType {
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
