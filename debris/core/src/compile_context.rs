use crate::{
    objects::{ClassRef, ObjStaticInt},
    Config, ObjectPayload, ObjectRef, ValidPayload,
};
use once_cell::unsync::OnceCell;
use std::{any::TypeId, cell::RefCell, collections::HashMap, default::Default, rc::Rc};

/// The Compilation context stores various information about the current compilation
#[derive(Debug, Eq, PartialEq)]
pub struct CompileContext {
    /// Contains all types
    pub type_ctx: TypeContext,
    /// The current config which specifies how to compile
    pub config: Rc<Config>,
}

impl Default for CompileContext {
    fn default() -> Self {
        CompileContext {
            config: Rc::new(Config::default()),
            type_ctx: TypeContext::default(),
        }
    }
}

/// Used to manage types
#[derive(Debug, Eq, PartialEq, Default)]
pub struct TypeContext {
    /// Cache for classes
    ///
    /// The key is the type of the Payload Struct.
    cache: RefCell<HashMap<TypeId, ClassRef>>,
    /// The null signleton
    null: OnceCell<ObjectRef>,
}

impl TypeContext {
    pub fn insert<T: ObjectPayload>(&self, class: ClassRef) {
        self.cache.borrow_mut().insert(TypeId::of::<T>(), class);
    }

    pub fn get<T: ObjectPayload>(&self) -> Option<ClassRef> {
        self.cache.borrow().get(&TypeId::of::<T>()).cloned()
    }

    #[inline]
    pub fn null(&self, compile_context: &CompileContext) -> ObjectRef {
        self.null
            .get_or_init(|| ObjStaticInt::new(0).into_object(compile_context))
            .clone()
    }
}
