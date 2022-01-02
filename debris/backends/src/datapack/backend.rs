use datapack_common::vfs::Directory;
use debris_common::CompileContext;
use debris_llir::Llir;

use crate::Backend;

use super::generator::DatapackGenerator;

/// The Datapack Backend implementation
#[derive(Debug, Default)]
pub struct DatapackBackend;

impl Backend for DatapackBackend {
    fn generate(&self, llir: &Llir, ctx: &CompileContext) -> Directory {
        DatapackGenerator::new(ctx, llir).build()
    }
}
