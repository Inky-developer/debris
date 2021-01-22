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

const DEBRIS_FILE_EXTENSION: &str = ".de";

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
            source: fs::read_to_string(root.join(&input_file)).expect("Could not read the input"),
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

        if let Err(e) = path.metadata() {
            Err(LangError::new(
                LangErrorKind::MissingModule {
                    path,
                    error: e.kind(),
                },
                span,
            )
            .into())
        } else {
            Ok(path)
        }
    }

    /// Locates the corresponding file, parses it and returns it as a [debris_core::hir::hir_nodes::HirModule]
    pub fn resolve_module(&mut self, module_name: String, span: Span) -> Result<HirModule> {
        let file_path = self.locate_module(module_name, span)?;

        let file_contents = match fs::read_to_string(&file_path) {
            Ok(val) => val,
            Err(err) => {
                return Err(LangError::new(
                    LangErrorKind::MissingModule {
                        path: file_path,
                        error: err.kind(),
                    },
                    span,
                )
                .into());
            }
        };

        let id = self.compile_context.add_input_file(Code {
            path: Some(file_path),
            source: file_contents,
        });

        let code_ref = self.compile_context.input_files.get_code_ref(id);
        let hir = Hir::from_code(code_ref, &self.compile_context)?;

        let module = HirModule {
            attributes: Vec::new(),
            block: hir.main_function,
            ident: span.into(),
            span,
        };

        if !hir.dependencies.is_empty() {
            // Note to also check for recursive imports when implementing that bs
            todo!("Implement transitive dependencies");
        }

        Ok(module)
    }

    pub fn get_hir(&mut self) -> Result<Hir> {
        let mut hir = Hir::from_code(
            self.compile_context.input_files.get_code_ref(0),
            &self.compile_context,
        )?;

        let mut imported_modules = Vec::new();
        for (module_name, span) in hir.dependencies.iter() {
            let module = self.resolve_module(module_name.to_string(), span)?;
            imported_modules.push(module);
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
