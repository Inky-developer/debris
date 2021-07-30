use debris_common::Ident;
use rustc_hash::FxHashMap;
use std::{
    any::Any,
    cmp::{Eq, PartialEq},
    fmt::{self, Debug, Display},
    ops::Deref,
    rc::Rc,
};

use crate::{
    class::ClassRef,
    objects::{obj_class::HasClass, obj_function::ObjFunction},
};

use super::CompileContext;
use crate::llir::memory::MemoryLayout;

/// The type of the properties map
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
/// Because [DebrisObject] is unsized, it generally only accessed as [ObjectRef]
pub struct DebrisObject<T: ObjectPayload + ?Sized> {
    /// The class of the object
    pub class: ClassRef,
    /// The actual value
    pub payload: T,
}

/// A trait for values that can be used as debris object payload
///
/// The private AsAny trait is auto-implemented
pub trait ObjectPayload: ValidPayload {
    /// Returns the memory layout of this specific object
    /// This method is usually only called once
    fn memory_layout(&self) -> &MemoryLayout;

    /// The class specific to this object.
    /// Contains additionally to the class generics and the memory layout
    ///
    /// Per default the default class of the object type
    fn create_class(&self, ctx: &CompileContext) -> ClassRef {
        self.get_class(ctx)
    }

    /// May be overwritten by distinct payloads which carry properties
    fn get_property(&self, _ctx: &CompileContext, _ident: &Ident) -> Option<ObjectRef> {
        None
    }

    /// Interface any object can implement to be treated as a function
    fn as_function(&self) -> Option<&ObjFunction> {
        None
    }
}

// Automatically implemented for every supported type
pub trait ValidPayload: Debug + Display + HasClass + 'static {
    fn as_any(&self) -> &dyn Any;

    /// Tests whether this object is equal to another object
    fn eq(&self, other: &ObjectRef) -> bool;

    fn into_object(self, ctx: &CompileContext) -> ObjectRef;

    fn get_class(&self, ctx: &CompileContext) -> ClassRef;
}

// Wow, thats a recursive dependency (ObjectPayload requires ValidPayload which requires ObjectPayload)
// Cool that it works
impl<T: Any + Debug + Display + PartialEq + Eq + ObjectPayload + HasClass> ValidPayload for T {
    fn as_any(&self) -> &dyn Any {
        self as &dyn Any
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        ObjectRef::from_payload(ctx, self)
    }

    fn get_class(&self, ctx: &CompileContext) -> ClassRef {
        Self::class(ctx)
    }
}

impl ObjectRef {
    pub fn from_payload<T: ObjectPayload>(ctx: &CompileContext, value: T) -> Self {
        ObjectRef(Rc::new(DebrisObject {
            class: value.create_class(ctx),
            payload: value,
        }))
    }
}

impl DebrisObject<dyn ObjectPayload> {
    /// Tries to get a property that belongs to this object
    ///
    /// First tries to retrieve the property from its payload.
    /// If that fails, tries to retrieve the property from its class.
    pub fn get_property(&self, ctx: &CompileContext, ident: &Ident) -> Option<ObjectRef> {
        self.payload
            .get_property(ctx, ident)
            .or_else(|| self.class.get_property(ctx, ident))
    }

    /// Converts the payload into its original type
    ///
    /// Returns None if the downcast is not possible
    pub fn downcast_payload<T: ObjectPayload>(&self) -> Option<&T> {
        self.payload.as_any().downcast_ref::<T>()
    }
}

impl Debug for DebrisObject<dyn ObjectPayload> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DebrisObject")
            // Why does it work with a double ref???
            .field("payload", &&self.payload)
            .field("class", &format_args!("{}", self.class))
            .finish()
    }
}

impl fmt::Display for ObjectRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Obj({})", &self.payload)
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
