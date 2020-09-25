use std::rc::Rc;

use debris_core::{llir::LLIR, Config};
use vfs::Directory;

mod common;
mod datapack;
pub use datapack::DatapackBackend;

pub trait Backend: Default {
    fn handle_llir(&mut self, llir: &LLIR) -> Directory;

    fn new(config: Rc<Config>) -> Self;

    fn generate(llir: &LLIR) -> Directory {
        Self::new(llir.config.clone()).handle_llir(llir)
    }
}
