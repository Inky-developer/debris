use rustc_hash::FxHashMap;

use crate::error::Result;
use crate::hir::Hir;
use crate::mir::mir_builder::MirBuilder;
use crate::mir::mir_context::{MirContext, MirContextId};
use crate::mir::namespace::MirNamespace;
use crate::CompileContext;

mod mir_builder;
mod mir_context;
mod mir_nodes;
mod mir_object;
mod namespace;

#[derive(Debug)]
pub struct Mir {
    entry_context: MirContextId,
    contexts: FxHashMap<MirContextId, MirContext>,
    namespace: MirNamespace,
}

impl Mir {
    pub fn new(ctx: &CompileContext, hir: &Hir) -> Result<Self> {
        let builder = MirBuilder::new(ctx, hir);
        builder.build()
    }
}
