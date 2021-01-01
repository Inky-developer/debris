//! Contains all objects which can be used in a debris program.

mod obj_bool;
pub use obj_bool::ObjBool;
mod obj_bool_static;
pub use obj_bool_static::ObjStaticBool;
mod obj_class;
pub use obj_class::{ClassRef, HasClass, ObjClass};
mod obj_function;
pub use obj_function::{FunctionContext, FunctionParameters, FunctionSignature, ObjFunction};
mod obj_native_function;
pub use obj_native_function::{
    FunctionParameterDefinition, ObjNativeFunction, ObjNativeFunctionSignature,
};
mod obj_integer_static;
pub use obj_integer_static::ObjStaticInt;
mod obj_integer;
pub use obj_integer::ObjInt;
mod obj_module;
pub use obj_module::{ModuleFactory, ObjModule};
mod obj_null;
pub use obj_null::ObjNull;
mod obj_string;
pub use obj_string::ObjString;
