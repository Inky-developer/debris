use debris_common::Ident;
use rustc_hash::FxHashMap;
use std::any::Any;
use std::cmp::{Eq, PartialEq};
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

use crate::objects::ClassRef;

use super::CompileContext;

pub type ObjectProperties = FxHashMap<Ident, ObjectRef>;

/// This struct is used to pass objects arround
///
/// Objects references are read-only, so values that can be modified must be declared in a cell.
#[derive(Debug, Clone)]
pub struct ObjectRef(Rc<DebrisObject<dyn ObjectPayload>>);

/// Objects are a central type for the compiler.
/// Basically anything that can be assigned to a variable is an object.
/// This includes numbers, function, modules, and more.
/// It is possible to cast the ObjectPayload to its original value.
pub struct DebrisObject<T: ObjectPayload + ?Sized> {
    /// The class of the object
    pub class: ClassRef,
    /// The actual value
    pub payload: T,
}

/// A trait for values that can be used as debris object payload
///
/// The private AsAny trait is auto-implemented
pub trait ObjectPayload: AsAny {
    /// Converts this payload into an object. ToDo: Auto implement this
    fn into_object(self, ctx: &CompileContext) -> ObjectRef;

    /// Tests whether this object is equal to another object
    fn eq(&self, other: &ObjectRef) -> bool;

    /// May be overwritten by distinct payloads which carry properties
    fn get_property(&self, _: &Ident) -> Option<ObjectRef> {
        None
    }
}

// Automatically implemented for every supported type
pub trait AsAny: Debug + 'static {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any + Debug> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self as &dyn Any
    }
}

impl DebrisObject<dyn ObjectPayload> {
    /// Creates a new Object and returns a reference to it
    pub fn new_ref<T: ObjectPayload>(class: ClassRef, payload: T) -> ObjectRef {
        DebrisObject { class, payload }.into()
    }

    /// Tries to get a property that belongs to this object
    ///
    /// First tries to retrieve the property from its payload.
    /// If that fails, tries to retrieve the property from its class.
    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.payload
            .get_property(ident)
            .or_else(|| self.class.get_property(ident))
    }

    /// Converts the payload into its original type
    ///
    /// Returns None if the downcast is not possible
    pub fn downcast_payload<T: ObjectPayload>(&self) -> Option<&T> {
        self.payload.as_any().downcast_ref::<T>()
    }

    /// Returns whether the payload is of type `T`
    pub fn is_instance<T: ObjectPayload>(&self) -> bool {
        self.payload.as_any().is::<T>()
    }
}

impl Debug for DebrisObject<dyn ObjectPayload> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DebrisObject")
            // Why does it work with a double ref???
            .field("payload", &&self.payload)
            .field("class", &format_args!("[...]"))
            .finish()
    }
}

impl PartialEq for ObjectRef {
    fn eq(&self, other: &ObjectRef) -> bool {
        self.payload.eq(other)
    }
}

impl Eq for ObjectRef {}

impl<T: ObjectPayload> From<DebrisObject<T>> for ObjectRef {
    fn from(object: DebrisObject<T>) -> Self {
        ObjectRef(Rc::new(object))
    }
}

impl Deref for ObjectRef {
    type Target = DebrisObject<dyn ObjectPayload>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
