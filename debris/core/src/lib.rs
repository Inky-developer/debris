#[macro_use]
extern crate pest_derive;

pub mod hir;
pub mod llir;
pub mod mir;

mod compile_context;
pub use compile_context::CompileContext;

mod database;
pub use database::DatabaseStruct;

mod inputs;
pub use inputs::{Inputs, InputsStorage};

mod debris_object;
pub use debris_object::{DebrisObject, ObjectPayload, ObjectProperties, ObjectRef, TemplateRef};
pub mod objects;

mod config;
pub use config::{BuildMode, Config};

pub mod error;
