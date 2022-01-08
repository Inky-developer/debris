//! Low-level intermediate representation
//!
//! The llir is quite close to minecraft functions.
pub mod block_id;
pub mod class;
mod debris_object;
mod error_utils;
pub mod extern_item_path;
pub mod function_interface;
pub mod item_id;
pub mod json_format;
mod llir_builder;
mod llir_function_builder;
mod llir_impl;
pub mod llir_nodes;
mod macro_impl_class;
mod memory;
pub mod minecraft_utils;
pub mod objects;
pub(crate) mod opt;
mod runtime;
pub mod type_context;
mod types;

pub use debris_object::{DebrisObject, ObjectPayload, ObjectProperties, ObjectRef, ValidPayload};
pub use llir_builder::NativeFunctionId;
pub use llir_impl::Llir;
pub use opt::call_graph::CallGraph;
pub use runtime::Runtime;
pub use types::{Type, TypePattern};
