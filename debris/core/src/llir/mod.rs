//! Low-level intermediate representation
//!
//! The llir is quite close to minecraft functions.
//! Also, no more MirTemplates exist, all objects are now computed.
//!
//! ToDo: Example for Llir pseudo-code

mod llir_context;
pub(crate) use llir_context::LlirContext;

mod llir_builder;
pub(crate) use llir_builder::LlirBuilder;

mod llir_impl;
pub use llir_impl::Llir;
pub(crate) use llir_impl::LlirFunctions;

pub(crate) mod opt;

pub mod llir_nodes;

pub mod utils;

pub mod json_format;

mod runtime;
pub use runtime::Runtime;
