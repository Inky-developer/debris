use std::{fs, path::PathBuf};

use debris_common::Code;
use debris_core::{
    error::Result,
    hir::Hir,
    llir::Llir,
    mir::{Mir, MirContextMap, NamespaceArena},
    objects::ModuleFactory,
    CompileContext,
};

pub struct CompileConfig {
    pub extern_modules: Vec<ModuleFactory>,
    pub compile_context: CompileContext,
}

impl CompileConfig {
    pub fn new(input_file: impl Into<PathBuf>, extern_modules: Vec<ModuleFactory>) -> Self {
        let input_file = input_file.into();
        let mut compile_context = CompileContext::default();
        compile_context.add_input_file(Code {
            source: fs::read_to_string(&input_file).expect("Could not read the input"),
            path: Some(input_file),
        });

        CompileConfig {
            extern_modules,
            compile_context,
        }
    }

    pub fn get_hir(&self) -> Result<Hir> {
        Hir::from_code(
            self.compile_context.input_files.code_ref(0),
            &self.compile_context,
        )
    }

    pub fn get_mir<'a>(&'a self, hir: &'a Hir) -> Result<Mir<'a>> {
        Mir::from_hir(hir, &self.compile_context, &self.extern_modules)
    }

    pub fn get_llir(
        &self,
        contexts: &MirContextMap,
        namespaces: &mut NamespaceArena,
    ) -> Result<Llir> {
        Llir::from_mir(contexts, namespaces)
    }
}
