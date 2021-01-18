use crate::{
    objects::{
        obj_bool::ObjBool,
        obj_bool_static::ObjStaticBool,
        obj_class::{ClassRef, HasClass, ObjClass},
        obj_int::ObjInt,
        obj_int_static::ObjStaticInt,
        obj_module::ObjModule,
        obj_native_function::ObjNativeFunction,
        obj_null::ObjNull,
        obj_string::ObjString,
    },
    Config, ObjectPayload, ObjectRef, Type, ValidPayload,
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
    type_ctx: TypeContext,
    /// The current config which specifies how to compile
    pub config: Rc<Config>,
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
    pub fn get_unique_id(&self) -> usize {
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

    /// Gets the class that corresponds to this type
    pub fn from_type(&self, value: Type) -> ClassRef {
        macro_rules! type_to_class {
            ($val:expr, $($ty:pat => $cls:ty),*,) => {
                match $val {
                    $(
                        $ty => <$cls>::class(self.ctx)
                    ),*
                }
            };
        }

        type_to_class! {value,
            Type::Null        => ObjNull,
            Type::StaticInt   => ObjStaticInt,
            Type::DynamicInt  => ObjInt,
            Type::StaticBool  => ObjStaticBool,
            Type::DynamicBool => ObjBool,
            Type::String      => ObjString,
            Type::Function    => ObjNativeFunction,
            Type::Class       => ObjClass,
            Type::Module      => ObjModule,
        }
    }
}
