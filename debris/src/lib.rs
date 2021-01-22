use std::{collections::HashSet, fs, path::PathBuf};

use debris_common::{Code, CodeId, Span};
use debris_core::{
    error::{LangError, LangErrorKind, Result},
    hir::{hir_nodes::HirModule, Hir, HirFile, ImportDependencies},
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
        let input_file = root
            .join(input_file.into())
            .canonicalize()
            .expect("Input file not found");

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
            Ok(path.canonicalize().unwrap())
        }
    }

    /// Locates the corresponding file, parses it and returns it as a [debris_core::hir::hir_nodes::HirModule]
    pub fn resolve_module(
        &mut self,
        dependencies: &mut ImportDependencies,
        module_name: String,
        span: Span,
    ) -> Result<(HirModule, CodeId)> {
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

        // Whether this import is the original input file
        let is_input_file = self
            .compile_context
            .input_files
            .get_input(0)
            .path
            .as_deref()
            .map_or(false, |input_path| input_path == file_path);

        let id = if is_input_file {
            0
        } else {
            self.compile_context.add_input_file(Code {
                path: Some(file_path),
                source: file_contents,
            })
        };

        let code_ref = self.compile_context.input_files.get_code_ref(id);
        let hir_file = HirFile::from_code(code_ref, &self.compile_context, dependencies)?;

        let module = HirModule {
            attributes: Vec::new(),
            block: hir_file.main_function,
            ident: span.into(),
            span,
        };

        Ok((module, id))
    }

    pub fn get_hir(&mut self) -> Result<Hir> {
        let mut dependency_list = ImportDependencies::default();
        let hir_file = HirFile::from_code(
            self.compile_context.input_files.get_code_ref(0),
            &self.compile_context,
            &mut dependency_list,
        )?;

        let mut visited_files = HashSet::new();
        visited_files.insert(0);

        let mut imported_modules = Vec::new();
        let mut i = 0;
        while i < dependency_list.len() {
            let (module_name, span) = dependency_list.get(i);
            let module_string = module_name.to_string();
            let (module, id) = self.resolve_module(&mut dependency_list, module_string, span)?;

            let is_new = visited_files.insert(id);
            if !is_new {
                return Err(LangError::new(
                    LangErrorKind::CircularImport {
                        module: dependency_list.get(i).0.to_string(),
                    },
                    span,
                )
                .into());
            }

            imported_modules.push(module);

            i += 1;
        }

        Ok(Hir {
            code_id: hir_file.code_id,
            main_function: hir_file.main_function,
            imported_modules,
        })
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
