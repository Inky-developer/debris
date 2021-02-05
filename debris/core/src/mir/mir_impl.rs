use super::{
    mir_context::NamespaceArena, ContextId, MirBuilder, MirContext, MirContextInfo, MirContextMap,
};

use crate::{
    error::Result,
    hir::{Hir, HirVisitor},
    objects::obj_module::ModuleFactory,
    CompileContext,
};

/// A Mid-level intermediate representation
#[derive(Debug, Default)]
pub struct Mir<'ctx> {
    /// All contexts
    ///
    /// A context can be for example a function body
    pub contexts: MirContextMap<'ctx>,
    pub namespaces: NamespaceArena,
}

impl<'ctx> Mir<'ctx> {
    pub fn context_info<'b>(&'b mut self, id: ContextId) -> MirContextInfo<'b, 'ctx> {
        MirContextInfo {
            context: self.contexts.get_mut(id),
            arena: &mut self.namespaces,
        }
    }

    pub fn add_context(&mut self, context: MirContext<'ctx>) {
        self.contexts.contexts.insert(context.id, context);
    }

    /// Converts the hir into a mir
    ///
    /// extern_modules: A slice of [ModuleFactory], which when called return a module object
    pub fn from_hir(
        hir: &Hir,
        compile_context: &'ctx CompileContext,
        extern_modules: &[ModuleFactory],
    ) -> Result<Mir<'ctx>> {
        let mut mir = Mir::default();

        let code_ref = compile_context.input_files.get_code_ref(hir.code_id);
        let mut builder = MirBuilder::new(
            &mut mir,
            &hir.imported_modules,
            extern_modules,
            compile_context,
            code_ref,
        );
        let main_function = &hir.main_function;
        builder.visit_block(main_function)?;

        mir.contexts.main_context = builder.main_context;

        Ok(mir)
    }
}
