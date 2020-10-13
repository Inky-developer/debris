//! Contains all objects which can be used in a debris program.

mod obj_class;
pub use obj_class::{ClassRef, HasClass, ObjectClass};
mod obj_function;
pub use obj_function::{
    CallbackFunction, FunctionContext, FunctionSignature, FunctionSignatureMap, ObjectFunction,
};
mod obj_integer_static;
pub use obj_integer_static::StaticInt;
mod obj_integer_dyn;
pub use obj_integer_dyn::DynInt;
mod obj_module;
pub use obj_module::{ModuleFactory, ObjectModule};
mod obj_string;
pub use obj_string::ObjectString;
