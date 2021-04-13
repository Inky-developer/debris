//! Core implementation of the debris compiler
//!
//! This crate contains most of the compilation logic.
//!
//! Compiling a debris script uses three different intermediate representations:
//! First of all, the text is parsed and a ast-like structure gets produced. This happens in the [hir module](debris_core::hir).
//!
//! Subsequently, this structure gets lowered into a [mid-level intermediate representation](debris_core::mir).
//! The mir contains debris_objects.
//!
//! At last the mir will be converted into a [low-level intermediate representation](debris_core::llir).
//! The llir is quite similar to actual minecraft command syntax and contains various nodes for scoreboard operations and more.
//!
//! A very important part of the compiler are [objects](debris_core::DebrisObject).
//! Objects represent anything that can be assigned to a variable in the debris language.

#[macro_use]
extern crate pest_derive;

pub mod hir;
pub mod llir;
pub mod mir;

mod class;
mod compile_context;
pub use compile_context::CompileContext;

mod namespace;
pub use namespace::Namespace;

mod debris_object;
pub use debris_object::{DebrisObject, ObjectPayload, ObjectProperties, ObjectRef, ValidPayload};
pub mod objects;

mod config;
pub use config::{BuildMode, Config, OptMode};

pub mod error;

mod types;
pub use types::{Type, TypePattern};

pub mod function_interface;
pub mod memory;

// Workaround for proc macros
extern crate self as debris_core;
