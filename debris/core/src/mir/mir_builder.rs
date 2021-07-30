use rustc_hash::FxHashMap;

use crate::error::Result;
use crate::hir::hir_nodes::HirBlock;
use crate::hir::Hir;
use crate::mir::mir_context::{MirContext, MirContextId};
use crate::mir::namespace::MirNamespace;
use crate::mir::Mir;
use crate::CompileContext;

pub struct MirBuilder<'ctx, 'hir> {
    compile_context: &'ctx CompileContext,
    hir: &'hir Hir,
    current_context: MirContext,
    entry_context: MirContextId,
    contexts: FxHashMap<MirContextId, MirContext>,
    namespace: MirNamespace,
    next_context_id: u32,
}

impl<'ctx, 'hir> MirBuilder<'ctx, 'hir> {
    pub fn new(ctx: &'ctx CompileContext, hir: &'hir Hir) -> Self {
        let entry_context = MirContextId {
            compilation_id: ctx.compilation_id,
            id: 0,
        };

        MirBuilder {
            compile_context: ctx,
            hir,
            current_context: MirContext::new(entry_context),
            entry_context,
            contexts: Default::default(),
            namespace: MirNamespace::new(ctx),
            next_context_id: 1,
        }
    }

    pub fn build(mut self) -> Result<Mir> {
        self.handle_block(&self.hir.main_function)?;

        Ok(Mir {
            namespace: self.namespace,
            entry_context: self.entry_context,
            contexts: self.contexts,
        })
    }
}

impl MirBuilder<'_, '_> {
    pub fn handle_block(&mut self, block: &HirBlock) -> Result<()> {
        todo!()
    }
}
