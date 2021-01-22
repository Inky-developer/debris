use std::{fs, path::PathBuf};

use debris_common::{Code, Span};
use debris_core::{
    error::{LangError, LangErrorKind, Result},
    hir::{hir_nodes::HirModule, Hir},
    llir::Llir,
    mir::{Mir, MirContextMap, NamespaceArena},
    objects::obj_module::ModuleFactory,
    CompileContext,
};

const DEBRIS_FILE_EXTENSION: &'static str = ".de";

pub struct CompileConfig {
    pub extern_modules: Vec<ModuleFactory>,
    pub compile_context: CompileContext,
    /// The root directoy of this compile run
    pub root: PathBuf,
}

impl CompileConfig {
    pub fn new(
        input_file: impl Into<PathBuf>,
        extern_modules: Vec<ModuleFactory>,
        root: PathBuf,
    ) -> Self {
        let input_file = input_file.into();
        let mut compile_context = CompileContext::default();
        compile_context.add_input_file(Code {
            source: fs::read_to_string(&input_file).expect("Could not read the input"),
            path: Some(input_file),
        });

        CompileConfig {
            extern_modules,
            compile_context,
            root,
        }
    }

    /// Locates a module and returns the path
    pub fn locate_module(&self, module_name: String, span: Span) -> Result<PathBuf> {
        let mut file_name = module_name;
        file_name.push_str(DEBRIS_FILE_EXTENSION);
        let path = self.root.join(file_name);

        // if let Err(e) = path.metadata() {
        //     return Err(LangError::new(LangErrorKind::, span))
        // }

        todo!()
    }

    /// Locates the corresponding file, parses it and returns it as a [debris_core::hir::hir_nodes::HirModule]
    pub fn resolve_module(&mut self, module_name: String, span: Span) -> Result<HirModule> {
        todo!()
    }

    pub fn get_hir(&mut self) -> Result<Hir> {
        let mut hir = Hir::from_code(
            self.compile_context.input_files.get_code_ref(0),
            &self.compile_context,
        )?;

        let mut i = 0;
        let mut imported_modules = Vec::new();
        while i < hir.dependencies.len() {
            for (module_name, span) in hir.dependencies.iter().nth(i) {
                let module = self.resolve_module(module_name.to_string(), span)?;
                imported_modules.push(module);
            }
            i = hir.dependencies.len() - 1;
        }

        hir.imported_modules = imported_modules;

        Ok(hir)
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
