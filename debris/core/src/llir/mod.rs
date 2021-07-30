//! Low-level intermediate representation
//!
//! The llir is quite close to minecraft functions.
//! Also, no more MirTemplates exist, all objects are now computed.

mod llir_impl;
pub use llir_impl::Llir;

pub(crate) mod opt;
pub use opt::call_graph::CallGraph;

pub mod llir_nodes;

pub mod utils;

pub mod json_format;

pub mod memory;
mod runtime;

pub use runtime::Runtime;
