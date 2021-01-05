use super::{
    mir_context::NamespaceArena, ContextId, MirBuilder, MirContext, MirContextInfo, MirContextMap,
};

use crate::{
    error::Result,
    hir::{Hir, HirVisitor},
    objects::ModuleFactory,
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
    pub fn context<'b>(&'b mut self, id: ContextId) -> MirContextInfo<'b, 'ctx> {
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
        hir: &Hir<'ctx>,
        compile_context: &'ctx CompileContext,
        extern_modules: &[ModuleFactory],
    ) -> Result<Mir<'ctx>> {
        let mut mir = Mir::default();

        let mut builder = MirBuilder::new(&mut mir, extern_modules, compile_context, hir.code_ref);
        let main_function = &hir.main_function;
        builder.visit_block(main_function)?;

        mir.contexts.main_context = builder.main_context;

        Ok(mir)
    }
}
