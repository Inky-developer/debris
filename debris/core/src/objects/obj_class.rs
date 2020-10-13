use std::{any::TypeId, cell::RefCell, rc::Rc};

use debris_common::Ident;

use crate::{
    compile_context::TypeContext, CompileContext, DebrisObject, ObjectPayload, ObjectProperties,
    ObjectRef, Type,
};

/// A reference to a class
pub type ClassRef = Rc<ObjectClass>;

pub trait HasClass: ObjectPayload {
    /// Returns the class of this object
    ///
    /// Usually auto-implement by the proc macro `#[object]`
    fn class(ctx: &CompileContext) -> ClassRef;
}

/// The class of a value.
///
/// Contains all associated methods
#[derive(Debug, Eq, PartialEq)]
pub struct ObjectClass {
    typ: Type,
    properties: RefCell<ObjectProperties>,
}

impl ObjectPayload for ObjectClass {
    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new_ref(self.class(&ctx.type_ctx), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |value| value == self)
    }
}

impl ObjectClass {
    /// Constructs a new class with a `typ` and class properties
    pub fn new(typ: Type, properties: ObjectProperties) -> Self {
        ObjectClass {
            typ,
            properties: properties.into(),
        }
    }

    /// Retrieves a property of this class
    pub fn get_property(&self, property: &Ident) -> Option<ObjectRef> {
        self.properties.borrow().get(property).cloned()
    }

    /// Changes a property of this class
    pub fn set_property(&self, key: Ident, value: ObjectRef) {
        self.properties.borrow_mut().insert(key, value);
    }

    /// Constructs a new class with a `typ`
    pub fn new_empty(typ: Type) -> Self {
        Self::new(typ, ObjectProperties::default())
    }

    /// Returns whether this class is the same class as `other`
    pub fn is(&self, other: &ObjectClass) -> bool {
        self.typ == other.typ
    }

    /// Whether this class matches the other class
    ///
    /// Currently the behaviour is the same as [ObjectClass::is].
    /// When the type system gets more sophisticated, this function can also match
    /// against things like interfaces, eg. `a.matches(b)` is true if
    /// a: StaticInt and b: Integer, where b is a generic interface for integers
    pub fn matches(&self, other: &ObjectClass) -> bool {
        self.is(other)
    }

    pub fn typ(&self) -> Type {
        self.typ
    }

    /// The class of this object, so basically the metaclass
    fn class(&self, ctx: &TypeContext) -> ClassRef {
        ctx.get_or_insert(TypeId::of::<Self>(), || ObjectClass::new_empty(Type::Class))
    }
}
