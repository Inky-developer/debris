//! Contains all objects which can be used in a debris program.

mod obj_class;
pub use obj_class::{ClassRef, HasClass, ObjClass};
mod obj_function;
pub use obj_function::{
    CallbackFunction, FunctionContext, FunctionSignature, FunctionSignatureMap, ObjFunction,
};
mod obj_integer_static;
pub use obj_integer_static::ObjStaticInt;
mod obj_integer_dyn;
pub use obj_integer_dyn::ObjInt;
mod obj_module;
pub use obj_module::{ModuleFactory, ObjModule};
mod obj_string;
pub use obj_string::ObjString;
