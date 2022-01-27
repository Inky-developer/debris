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
    class::ClassRef, json_format::JsonFormatComponent, memory::MemoryLayout,
    objects::obj_class::ObjClass,
};

use super::{objects::obj_class::HasClass, type_context::TypeContext};

/// The type of the properties map
pub type ObjectProperties = FxHashMap<Ident, ObjectRef>;

/// This struct is used to pass objects around
///
/// Objects references are read-only, so values that can be modified must be declared in a cell.
#[derive(Debug, Clone)]
pub struct ObjectRef(Rc<DebrisObject<dyn ObjectPayload>>);

/// Objects are a central type for the compiler.
/// Basically anything that can be assigned to a variable is an object.
/// This includes numbers, function, modules, and more.
/// It is possible to cast the `ObjectPayload` to its original value.
/// Because [`DebrisObject`] is unsized, it generally only accessed as [`ObjectRef`]
pub struct DebrisObject<T: ObjectPayload + ?Sized> {
    /// The class of the object
    pub class: ClassRef,
    /// The actual value
    pub payload: T,
}

/// A trait for values that can be used as debris object payload
///
/// The private [`ValidPayload`] trait is auto-implemented
pub trait ObjectPayload: ValidPayload {
    /// Returns the memory layout of this specific object
    /// This method is usually only called once
    fn memory_layout(&self) -> &MemoryLayout;

    /// The class specific to this object.
    /// Contains additionally to the class generics and the memory layout
    ///
    /// Per default the default class of the object type
    fn create_class(&self, ctx: &TypeContext) -> ClassRef {
        self.get_class(ctx)
    }

    /// Returns the class which the runtime encodable variant of this object would have.
    /// Used to determine the class passed into the `promote` method of objects.
    fn runtime_class(&self, _ctx: &TypeContext) -> Option<ClassRef> {
        None
    }

    /// May be overwritten by distinct payloads which carry properties
    fn get_property(&self, _ctx: &TypeContext, _ident: &Ident) -> Option<ObjectRef> {
        None
    }

    /// Converts this object into json components so it can be rendered in minecraft chat
    fn json_fmt(&self, buf: &mut Vec<JsonFormatComponent>) {
        buf.push(JsonFormatComponent::RawText(self.to_string().into()));
    }
}

// Automatically implemented for every supported type
pub trait ValidPayload: Debug + Display + HasClass + 'static {
    fn as_any(&self) -> &dyn Any;

    /// Tests whether this object is equal to another object
    fn eq(&self, other: &ObjectRef) -> bool;

    fn into_object(self, ctx: &TypeContext) -> ObjectRef;

    fn get_class(&self, ctx: &TypeContext) -> ClassRef;
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

    fn into_object(self, ctx: &TypeContext) -> ObjectRef {
        ObjectRef::from_payload(ctx, self)
    }

    fn get_class(&self, ctx: &TypeContext) -> ClassRef {
        Self::class(ctx)
    }
}

impl ObjectRef {
    pub fn from_payload<T: ObjectPayload>(ctx: &TypeContext, value: T) -> Self {
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
    pub fn get_property(&self, ctx: &TypeContext, ident: &Ident) -> Option<ObjectRef> {
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

    /// Helper function for downcasting the payload into a class
    pub fn downcast_class(&self) -> Option<ClassRef> {
        let class = self.downcast_payload::<ObjClass>();
        class.map(|class| Rc::clone(&class.class))
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
        let payload = &self.payload;
        write!(f, "{payload}")
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

/// Helper macro that makes match-style semantics available for duck typed objects
/// # Example
/// ```ignore
/// match_object!{obj,
///     string: ObjString => do_something_with_string(string),
///     int: ObjInt => do_something_with_int(int),
///     else => some_fallback_behavior(obj),
/// }
/// ```
#[macro_export]
macro_rules! match_object {
    ($obj:ident, ) => {{}};
    ($obj:ident, else => $expr:expr,) => {
        $expr
    };
    ($obj:ident, $name:ident: $type:ty => $expr:expr, $($rest:tt)*) => {
        if let Some($name) = $obj.downcast_payload::<$type>() {
            $expr
        } else {
            match_object!($obj, $($rest)*)
        }
    };
}
