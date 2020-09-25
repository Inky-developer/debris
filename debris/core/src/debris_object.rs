use debris_common::Ident;
use debris_type::Type;
use rustc_hash::FxHashMap;
use std::any::Any;
use std::cmp::{Eq, PartialEq};
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

use super::objects::ObjectTemplate;
use super::CompileContext;

pub type ObjectProperties = FxHashMap<Ident, ObjectRef>;
pub type TemplateRef = Rc<ObjectTemplate>;

#[derive(Debug, Clone)]
pub struct ObjectRef(Rc<DebrisObject<dyn ObjectPayload>>);

// A debris object
// #[derive(Debug)]
pub struct DebrisObject<T: ObjectPayload + ?Sized> {
    pub typ: Type,
    pub template: TemplateRef,
    pub properties: ObjectProperties,
    pub payload: T,
}

pub trait ObjectPayload: Any + Debug + 'static {
    fn as_any(&self) -> &dyn Any;

    fn typ(&self) -> Type;

    fn into_object(self, ctx: &CompileContext) -> ObjectRef;

    fn eq(&self, other: &ObjectRef) -> bool;
}

impl DebrisObject<dyn ObjectPayload> {
    pub fn new<'a, T: ObjectPayload>(template: TemplateRef, payload: T) -> ObjectRef {
        DebrisObject {
            typ: payload.typ(),
            template,
            properties: ObjectProperties::default(),
            payload,
        }
        .into()
    }

    pub fn with_properties<T: ObjectPayload>(
        template: TemplateRef,
        payload: T,
        properties: ObjectProperties,
    ) -> ObjectRef {
        DebrisObject {
            typ: payload.typ(),
            template,
            properties,
            payload,
        }
        .into()
    }

    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.properties
            .get(ident)
            .or_else(|| self.template.get_property(ident))
            .cloned()
    }

    pub fn downcast_payload<T: ObjectPayload>(&self) -> Option<&T> {
        self.payload.as_any().downcast_ref::<T>()
    }
}

impl Debug for DebrisObject<dyn ObjectPayload> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DebrisObject")
            .field("typ", &self.typ)
            .field("properties", &self.properties)
            // Why does it work with a double ref???
            .field("payload", &&self.payload)
            .field("template", &format_args!("[...]"))
            .finish()
    }
}

impl PartialEq for ObjectRef {
    fn eq(&self, other: &ObjectRef) -> bool {
        (&self.payload).eq(other)
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
