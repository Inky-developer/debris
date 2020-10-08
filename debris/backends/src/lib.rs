//! Backend implementations for the debris compiler
//!
//! A Backend trait has to handle [LLIR](debris_core::llir::LLIR) and produce a [Directory](vfs::Directory).
//!
//! The module [common] defines useful items that are specifc to minecraft.

// Clippy wants to return a `Rc<str>` but I have no clue how to construct that
#![allow(clippy::rc_buffer)]

use std::rc::Rc;

use debris_core::{llir::LLIR, Config};
use vfs::Directory;

pub mod common;
mod datapack;
pub use datapack::DatapackBackend;

/// A Backend for debris, which has to convert `LLIR` into a `Directory`
pub trait Backend: Default {
    /// Converts the llir into a directory
    fn handle_llir(&mut self, llir: &LLIR) -> Directory;

    /// Creates a new backend
    fn new(config: Rc<Config>) -> Self;

    /// Simplifies the api of backends, so that the backend
    /// will not have to be explicitely constructed
    fn generate(llir: &LLIR) -> Directory {
        Self::new(llir.config.clone()).handle_llir(llir)
    }
}
