use debris_common::Ident;
use debris_derive::template;
use debris_type::Type;
use std::{cell::RefCell, rc::Rc};

use crate::{CompileContext, DebrisObject};
use crate::{ObjectPayload, ObjectProperties, ObjectRef};

pub type TypeRef = Rc<ObjectType>;

/// The type of an object
///
/// Works similar to how classes work in other languages.
/// Basically, this object is the super class for other objects.
#[derive(Debug, Eq, PartialEq)]
pub struct ObjectType {
    /// The type of this class. Is a Template<...>
    typ: Type,
    /// The properties the class has
    properties: RefCell<ObjectProperties>,
    /// An optional meta class. Currently unused
    template: Option<TypeRef>,
}

#[template]
impl ObjectType {
    /// Returns a property
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

    /// Sets a property
    pub fn set_property(&self, ident: Ident, value: ObjectRef) {
        self.properties.borrow_mut().insert(ident, value);
    }

    /// Creates a new reference to a ObjectType
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

    // The type of the subclass
    pub fn value_typ(&self) -> &Type {
        &self.typ
    }
}

impl ObjectPayload for ObjectType {
    fn typ(&self) -> Type {
        Type::Type
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new_ref(ctx.type_ctx.template_for_type(&self.typ()), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
    }
}
