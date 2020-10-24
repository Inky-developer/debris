use std::{cell::RefCell, rc::Rc};

use debris_common::Ident;
use debris_derive::object;

use crate::{CompileContext, ObjectPayload, ObjectProperties, ObjectRef, Type};

/// A reference to a class
pub type ClassRef = Rc<ObjClass>;

/// Marks objects that have a class
///
/// Every object payload has to implement this trait.
pub trait HasClass {
    /// Returns the class of this object
    ///
    /// Usually auto-implement by the proc macro `#[object]`
    fn class(ctx: &CompileContext) -> ClassRef
    where
        Self: Sized;
}

/// The class of a value.
///
/// Contains all associated methods
#[derive(Debug, Eq, PartialEq)]
pub struct ObjClass {
    typ: Type,
    properties: RefCell<ObjectProperties>,
}

#[object(Type::Class)]
impl ObjClass {
    /// Constructs a new class with a `typ` and class properties
    pub fn new(typ: Type, properties: ObjectProperties) -> Self {
        ObjClass {
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
    pub fn is(&self, other: &ObjClass) -> bool {
        self.typ == other.typ
    }

    /// Whether this class matches the other class
    ///
    /// Currently the behaviour is the same as [ObjectClass::is].
    /// When the type system gets more sophisticated, this function can also match
    /// against things like interfaces, eg. `a.matches(b)` is true if
    /// a: StaticInt and b: Integer, where b is a generic interface for integers
    pub fn matches(&self, other: &ObjClass) -> bool {
        self.is(other)
    }

    pub fn typ(&self) -> Type {
        self.typ
    }
}

impl ObjectPayload for ObjClass {}
