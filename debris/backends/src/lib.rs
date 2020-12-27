//! Backend implementations for the debris compiler
//!
//! A Backend trait has to handle [LLIR](debris_core::llir::LLIR) and produce a [Directory](vfs::Directory).
//!
//! The module [common] defines useful items that are specifc to minecraft.

// // Clippy wants to return a `Rc<str>` but I have no clue how to construct that
// #![allow(clippy::rc_buffer)]

use debris_core::{llir::Llir, CompileContext};
use vfs::Directory;

pub mod common;
mod datapack;
pub use datapack::DatapackBackend;

/// A Backend for debris, which has to convert `LLIR` into a `Directory`
pub trait Backend<'a>: Sized {
    /// Converts the llir into a directory
    fn handle_llir(&mut self, llir: &Llir) -> Directory;

    /// Creates a new backend
    fn new(context: &'a CompileContext) -> Self;

    /// Simplifies the api of backends, so that the backend
    /// will not have to be explicitely constructed
    fn generate(llir: &Llir, ctx: &'a CompileContext) -> Directory {
        Self::new(ctx).handle_llir(llir)
    }
}
