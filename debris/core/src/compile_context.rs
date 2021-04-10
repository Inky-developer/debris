use crate::{
    objects::{obj_class::ClassRef, obj_never::ObjNever, obj_null::ObjNull},
    Config, ObjectPayload, ObjectRef, ValidPayload,
};
use debris_common::{Code, CodeId, InputFiles};
use once_cell::unsync::OnceCell;
use rustc_hash::FxHashMap;
use std::{
    any::TypeId,
    cell::{Cell, RefCell},
    default::Default,
    rc::Rc,
};

/// The Compilation context stores various information about the current compilation
#[derive(Debug, Default)]
pub struct CompileContext {
    /// Contains all types
    type_ctx: TypeContext,
    /// The current config which specifies how to compile
    pub config: Config,
    /// The code files
    pub input_files: InputFiles,
    /// The current unique id system.
    /// Note that this is different from ids that are used in mir and llir.
    current_uid: Cell<usize>,
}

impl CompileContext {
    pub fn type_ctx(&self) -> TypeContextRef {
        TypeContextRef {
            ctx: self,
            type_ctx: &self.type_ctx,
        }
    }

    pub fn add_input_file(&mut self, code: Code) -> CodeId {
        self.input_files.add_input(code)
    }

    pub fn get_input_file(&self, id: CodeId) -> &Code {
        self.input_files.get_input(id)
    }

    /// Returns a unique id
    ///
    /// ToDo: In order to implement concurrent parsing of dependencies,
    /// or just caching of objects, this entire system needs to be reworked.
    /// For example, the same function must always (somehow) get the same id
    pub fn get_unique_id(&self) -> usize {
        let old = self.current_uid.get();
        self.current_uid.set(old + 1);
        old
    }
}

/// This call just makes sure that no circular references exist
impl Drop for CompileContext {
    fn drop(&mut self) {
        for (_, class) in self.type_ctx.cache.borrow().iter() {
            debug_assert!(
                Rc::strong_count(class) == 1,
                "Detected a circular dependency which can lead to memory leaks"
            )
        }
    }
}

/// Used to manage types
#[derive(Debug, Eq, PartialEq, Default)]
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

/// Wrapper for ergonomics, holds a ref to type_ctx and compile_ctx
pub struct TypeContextRef<'a> {
    type_ctx: &'a TypeContext,
    ctx: &'a CompileContext,
}

impl TypeContextRef<'_> {
    pub fn insert<T: ObjectPayload>(&self, class: ClassRef) {
        self.type_ctx
            .cache
            .borrow_mut()
            .insert(TypeId::of::<T>(), class);
    }

    pub fn get<T: ObjectPayload>(&self) -> Option<ClassRef> {
        self.type_ctx
            .cache
            .borrow()
            .get(&TypeId::of::<T>())
            .cloned()
    }

    #[inline]
    pub fn null(&self) -> ObjectRef {
        self.type_ctx
            .null
            .get_or_init(|| ObjNull.into_object(self.ctx))
            .clone()
    }

    #[inline]
    pub fn never(&self) -> ObjectRef {
        self.type_ctx
            .never
            .get_or_init(|| ObjNever.into_object(self.ctx))
            .clone()
    }
}
