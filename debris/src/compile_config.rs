use std::collections::{HashMap, HashSet};

use debris_common::{
    file_provider::FileProvider, Code, CodeId, CompilationId, CompileContext, Ident, Span,
};
use debris_error::{CompileErrors, LangError, LangErrorKind};
use debris_hir::{hir_nodes::HirModule, Hir, HirFile, ImportDependencies};
use debris_llir::{type_context::TypeContext, Llir, ObjectRef};
use debris_mir::Mir;

pub struct CompileConfig {
    pub compile_context: CompileContext,
    pub file_provider: Box<dyn FileProvider>,
}

impl CompileConfig {
    pub fn new(file_provider: Box<dyn FileProvider>) -> Self {
        CompileConfig {
            compile_context: CompileContext::new(CompilationId(0)),
            file_provider,
        }
    }

    pub fn add_file(&mut self, path: &str) -> CodeId {
        let content = self
            .file_provider
            .read_file(path)
            .unwrap_or_else(|| panic!("Could not read file '{path}'"));
        self.compile_context.add_input_file(Code {
            path: Some(path.into()),
            source: content,
        })
    }

    /// Locates the corresponding file, parses it and returns it as a [`debris_hir::hir_nodes::HirModule`]
    pub fn resolve_module(
        &mut self,
        dependencies: &mut ImportDependencies,
        module_name: &str,
        span: Span,
    ) -> Result<(HirModule, CodeId), CompileErrors> {
        let module_name = format!("{module_name}.de");
        let file_id = if let Some(file_id) = self
            .compile_context
            .input_files
            .find_by_filename(&module_name)
        {
            file_id
        } else {
            let Some(file_contents) = self.file_provider.read_file(&module_name) else {
                return Err(vec![LangError::new(
                    LangErrorKind::MissingModule {
                        path: module_name,
                        error: std::io::ErrorKind::NotFound,
                    },
                    span,
                )
                .into()]
                .into());
            };
            self.compile_context.add_input_file(Code {
                path: Some(module_name.into()),
                source: file_contents,
            })
        };

        let code_ref = self.compile_context.input_files.get_code_ref(file_id);
        let hir_file = HirFile::from_code(code_ref, &self.compile_context, dependencies)?;

        let module = HirModule {
            attributes: Vec::new(),
            block: hir_file.main_function,
            ident: span.into(),
            span,
        };

        Ok((module, file_id))
    }

    pub fn compute_hir(&mut self, input_id: CodeId) -> Result<Hir, CompileErrors> {
        let mut dependency_list = ImportDependencies::default();
        let hir_file = HirFile::from_code(
            self.compile_context.input_files.get_code_ref(input_id),
            &self.compile_context,
            &mut dependency_list,
        )?;

        let mut visited_files = HashSet::new();
        visited_files.insert(input_id);

        let mut imported_modules = Vec::new();
        let mut i = 0;
        while i < dependency_list.len() {
            let (module_name, span) = dependency_list.get(i);
            let module_string = module_name.to_string();
            let (module, id) = self.resolve_module(&mut dependency_list, &module_string, span)?;

            let is_new = visited_files.insert(id);
            if !is_new {
                return Err(vec![LangError::new(
                    LangErrorKind::CircularImport {
                        module: dependency_list.get(i).0.to_string(),
                    },
                    span,
                )
                .into()]
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

    pub fn compute_mir(&self, hir: &Hir) -> Result<Mir, CompileErrors> {
        Mir::new(&self.compile_context, hir).map_err(|err| vec![err].into())
    }

    pub fn compute_llir(
        &self,
        mir: &Mir,
        extern_items_factory: impl Fn(&TypeContext) -> HashMap<Ident, ObjectRef>,
    ) -> Result<Llir, CompileErrors> {
        Llir::new(&self.compile_context, extern_items_factory, mir).map_err(|err| vec![err].into())
    }
}
