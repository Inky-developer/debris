//! Backend implementations for the debris compiler
//!
//! A Backend trait has to handle [Llir](debris_core::llir::Llir) and produce a [Directory](vfs::Directory).
//!
//! The module [common] defines useful items that are specific to minecraft.

use debris_common::CompileContext;
use debris_llir::Llir;
use vfs::Directory;

pub mod common;
mod datapack;
pub use datapack::DatapackBackend;

/// A Backend for debris, which has to convert `LLIR` into a `Directory`
pub trait Backend {
    /// Converts the llir into a directory
    fn generate(&self, llir: &Llir, ctx: &CompileContext) -> Directory;
}
