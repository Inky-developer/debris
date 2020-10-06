use debris_common::Ident;
use debris_type::Type;
use rustc_hash::FxHashMap;
use std::any::Any;
use std::cmp::{Eq, PartialEq};
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

use crate::objects::TypeRef;

use super::CompileContext;

pub type ObjectProperties = FxHashMap<Ident, ObjectRef>;

#[derive(Debug, Clone)]
pub struct ObjectRef(Rc<DebrisObject<dyn ObjectPayload>>);

// A debris object
pub struct DebrisObject<T: ObjectPayload + ?Sized> {
    pub typ: Type,
    pub template: TypeRef,
    pub payload: T,
}

// The private AsAny trait is auto-implemented
pub trait ObjectPayload: AsAny {
    /// The type of the value
    fn typ(&self) -> Type;

    fn into_object(self, ctx: &CompileContext) -> ObjectRef;

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
    pub fn new<'a, T: ObjectPayload>(template: TypeRef, payload: T) -> ObjectRef {
        DebrisObject {
            typ: payload.typ(),
            template,
            payload,
        }
        .into()
    }

    /// Tries to get a property that belongs to this object
    /// First tries to retrieve the property from its payloads
    /// If that fails, tries to retrieve the property from its payload
    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.payload
            .get_property(ident)
            .or_else(|| self.template.get_property(ident))
    }

    pub fn downcast_payload<T: ObjectPayload>(&self) -> Option<&T> {
        self.payload.as_any().downcast_ref::<T>()
    }

    pub fn is_instance<T: ObjectPayload>(&self) -> bool {
        self.payload.as_any().is::<T>()
    }
}

impl Debug for DebrisObject<dyn ObjectPayload> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DebrisObject")
            .field("typ", &self.typ)
            // Why does it work with a double ref???
            .field("payload", &&self.payload)
            .field("template", &format_args!("[...]"))
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
