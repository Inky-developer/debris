use crate::{
    objects::{ClassRef, ObjStaticInt},
    Config, ObjectPayload, ObjectRef, ValidPayload,
};
use debris_common::{Code, CodeId, InputFiles};
use once_cell::unsync::OnceCell;
use std::{
    any::TypeId,
    cell::{Cell, RefCell},
    collections::HashMap,
    default::Default,
    rc::Rc,
};

/// The Compilation context stores various information about the current compilation
#[derive(Debug, Default)]
pub struct CompileContext {
    /// Contains all types
    pub type_ctx: TypeContext,
    /// The current config which specifies how to compile
    pub config: Rc<Config>,
    /// The code files
    pub input_files: InputFiles,
    /// The current unique id system.
    /// Note that this is different from ids that are used in mir and llir.
    current_uid: Cell<u64>,
}

impl CompileContext {
    pub fn add_input_file(&mut self, code: Code) -> CodeId {
        self.input_files.add_input(code)
    }

    pub fn get_input_file(&self, id: CodeId) -> &Code {
        self.input_files.get_input(id)
    }

    /// Returns a unique id
    pub fn get_unique_id(&self) -> u64 {
        let old = self.current_uid.get();
        self.current_uid.set(old + 1);
        old
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
