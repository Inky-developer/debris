use std::{fs, path::PathBuf, rc::Rc};

use debris_common::{Code, CodeRef};
use debris_core::{
    error::Result,
    hir::Hir,
    llir::Llir,
    mir::{Mir, MirContext, NamespaceArena},
    objects::ModuleFactory,
    CompileContext,
};

pub struct CompileConfig {
    input_file: PathBuf,
    extern_modules: Vec<ModuleFactory>,
    compile_context: Rc<CompileContext>,
}

impl CompileConfig {
    pub fn new(input_file: impl Into<PathBuf>, extern_modules: Vec<ModuleFactory>) -> Self {
        CompileConfig {
            input_file: input_file.into(),
            extern_modules,
            compile_context: Rc::new(CompileContext::default()),
        }
    }

    pub fn get_file_content(&self) -> CodeRef {
        CodeRef::new(Code {
            path: Some(self.input_file.clone()),
            source: fs::read_to_string(&self.input_file).expect("Could not read input file"),
        })
    }

    pub fn get_hir(&self) -> Result<Hir> {
        Hir::from_code(self.get_file_content())
    }

    pub fn get_mir(&self, hir: &Hir) -> Result<Mir> {
        Mir::from_hir(hir, self.compile_context.clone(), &self.extern_modules)
    }

    pub fn get_llir(
        &self,
        contexts: &[MirContext],
        namespaces: &mut NamespaceArena,
    ) -> Result<Llir> {
        Llir::from_mir(contexts, namespaces, self.compile_context.config.clone())
    }
}
