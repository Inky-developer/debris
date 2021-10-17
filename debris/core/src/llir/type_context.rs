use std::{any::TypeId, cell::RefCell};

use once_cell::unsync::OnceCell;
use rustc_hash::FxHashMap;

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

    pub fn get<T: ObjectPayload>(&self) -> Option<ClassRef> {
        self.cache.borrow().get(&TypeId::of::<T>()).cloned()
    }

    pub fn insert<T: ObjectPayload>(&self, class: ClassRef) {
        self.cache.borrow_mut().insert(TypeId::of::<T>(), class);
    }
}
