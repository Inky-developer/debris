use std::fmt;

use debris_common::Ident;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::error::Result;
use crate::hir::Hir;
use crate::mir::mir_builder::MirBuilder;
use crate::mir::mir_context::{MirContext, MirContextId};
use crate::mir::namespace::MirNamespace;
use crate::CompileContext;

use self::mir_context::ReturnValuesArena;
use self::mir_object::MirObjectId;

mod mir_builder;
pub mod mir_context;
pub mod mir_nodes;
pub mod mir_object;
pub mod mir_primitives;
pub mod namespace;

pub struct Mir {
    pub entry_context: MirContextId,
    pub contexts: FxHashMap<MirContextId, MirContext>,
    pub namespace: MirNamespace,
    pub return_values_arena: ReturnValuesArena,
    pub extern_items: FxHashMap<Ident, MirObjectId>,
}

impl Mir {
    pub fn new(ctx: &CompileContext, hir: &Hir) -> Result<Self> {
        let builder = MirBuilder::new(ctx, hir);
        builder.build()
    }
}

impl fmt::Debug for Mir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.extern_items.is_empty() {
            writeln!(f, "Extern Items:")?;
            for (ident, id) in &self.extern_items {
                writeln!(f, "{:?} := {}", id, ident)?;
            }
        }

        writeln!(f)?;

        let main_context = &self.contexts[&self.entry_context];
        writeln!(f, "{:?}", main_context)?;

        for context in self.contexts.values().sorted_by_key(|ctx| ctx.id) {
            if context.id == self.entry_context {
                continue;
            }

            writeln!(f, "{:?}", context)?;
        }

        Ok(())
    }
}
