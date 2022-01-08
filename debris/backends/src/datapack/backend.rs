use datapack_common::vfs::Directory;
use debris_common::CompileContext;
use debris_llir::Llir;

use crate::Backend;

use super::generator::DatapackGenerator;

/// The Datapack Backend implementation
#[derive(Debug, Default)]
pub struct DatapackBackend;

impl DatapackBackend {
    pub const FILE_EXTENSION: &'static str = ".mcfunction";
    // Internal namespaces must be prefixed by double underscores, other namespaces may
    // be occupied by the user
    /// The directory which will contain all automatically generated minecraft files
    pub const FUNCTION_INTERNAL_PATH: &'static str = "__generated/";
}

impl Backend for DatapackBackend {
    fn generate(&self, llir: &Llir, ctx: &CompileContext) -> Directory {
        DatapackGenerator::new(ctx, llir).build()
    }
}
