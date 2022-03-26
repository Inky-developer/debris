use std::path::PathBuf;

use debris_common::{OptMode, file_provider::FsFileProvider};
use debris_lang::{common::Code, error::Result, llir::Llir, CompileConfig};

pub fn get_llir(config: &mut CompileConfig, input_id: usize) -> Result<Llir> {
    let high_ir = config.compute_hir(input_id)?;
    let medium_ir = config.compute_mir(&high_ir)?;
    config.compute_llir(&medium_ir, debris_std::load_all)
}

pub fn compile_file(file: &str, root: PathBuf) -> (Result<Llir>, CompileConfig) {
    let file_provider = FsFileProvider::new(root);
    let mut config = CompileConfig::new(Box::new(file_provider));
    let id = config.add_file(file);
    (get_llir(&mut config, id), config)
}

pub fn compile_string(
    source: Box<str>,
    root: PathBuf,
    opt_mode: OptMode,
) -> (Result<Llir>, CompileConfig) {
    let file_provider = FsFileProvider::new(root);
    let mut config = CompileConfig::new(Box::new(file_provider));
    let id = config
        .compile_context
        .add_input_file(Code { path: None, source });
    config.compile_context.config.opt_mode = opt_mode;
    (get_llir(&mut config, id), config)
}
