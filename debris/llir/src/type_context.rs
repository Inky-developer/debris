use std::cell::OnceCell;
use std::{any::TypeId, cell::RefCell};

use rustc_hash::FxHashMap;

use crate::objects::{obj_class::ObjClass, obj_never::ObjNever};

use super::{class::ClassRef, objects::obj_null::ObjNull, ObjectPayload, ObjectRef, ValidPayload};

/// Caches all classes. Could maybe be used as an arena in the future in cases the Rc design is not fast enough.
/// TODO: Right now the stored classes create cycles which lead to memory leaks. This should be fixed quickly.
#[derive(Debug, Default)]
pub struct TypeContext {
    /// Cache for classes
    ///
    /// The key is the type of the Payload Struct.
    cache: RefCell<FxHashMap<TypeId, ClassRef>>,
    /// The null singleton
    null: OnceCell<ObjectRef>,
    /// The never singleton
    never: OnceCell<ObjectRef>,
}

impl TypeContext {
    pub fn null(&self) -> ObjectRef {
        self.null.get_or_init(|| ObjNull.into_object(self)).clone()
    }

    pub fn never(&self) -> ObjectRef {
        self.never
            .get_or_init(|| ObjNever.into_object(self))
            .clone()
    }

    /// Returns an object ref to the static class of the given type
    pub fn static_class_obj<T: ObjectPayload>(&self) -> ObjectRef {
        ObjClass::new(T::static_class(self)).into_object(self)
    }

    pub fn get<T: ObjectPayload>(&self) -> Option<ClassRef> {
        self.cache.borrow().get(&TypeId::of::<T>()).cloned()
    }

    pub fn insert<T: ObjectPayload>(&self, class: ClassRef) {
        self.cache.borrow_mut().insert(TypeId::of::<T>(), class);
    }
}
