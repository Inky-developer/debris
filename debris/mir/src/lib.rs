use std::fmt;

use debris_common::{CompileContext, Ident, Span};
use debris_error::Result;
use debris_hir::Hir;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    mir_builder::MirBuilder,
    mir_context::{MirContext, MirContextId},
    namespace::MirNamespace,
};

use self::{mir_context::ReturnValuesArena, mir_object::MirObjectId};

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
    pub extern_items: FxHashMap<Ident, MirExternItem>,
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
            for (ident, item) in &self.extern_items {
                let object_id = item.object_id;
                writeln!(f, "{object_id:?} := {ident}")?;
            }
        }
        writeln!(f)?;

        for obj in &self.namespace.objects {
            let obj_namespace = self.namespace.get_local_namespace(obj.local_namespace_id);
            if !obj_namespace.is_empty() {
                writeln!(
                    f,
                    "{:?}: \n - {}\n",
                    obj.id,
                    obj_namespace
                        .iter()
                        .map(|(ident, id)| format!("{:?}: {}", id.0, ident))
                        .join("\n - ")
                )?;
            }
        }

        let main_context = &self.contexts[&self.entry_context];
        writeln!(f, "{:?}", main_context.debug(&self.return_values_arena))?;

        for context in self.contexts.values().sorted_by_key(|ctx| ctx.id) {
            if context.id == self.entry_context {
                continue;
            }

            writeln!(f, "{:?}", context.debug(&self.return_values_arena))?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct MirExternItem {
    pub definition_span: Span,
    pub object_id: MirObjectId,
}
