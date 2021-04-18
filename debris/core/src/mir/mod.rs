//! Mid-level intermediate representation
//!
//! Once the high lever representation has be created, it can be lowered into a mir.
//! The mir is quite simplistic and focuses on control flow and types.
//!
//! An input of `let a = 3 * 5 + 2` would be represented as a bunch of `FunctionCall(...)` and look somewhat like this:
//! ```pseudocode
//! Call(
//!     StaticInt.*,
//!     parameters: [StaticInt(3), StaticInt(5)],
//!     return_val: {id: 1, type: StaticInt}
//! );
//!
//! Call(
//!     StaticInt.+,
//!     parameters: [{id: 1, type: StaticInt}, StaticInt(2)],
//!     return_val {id: 2, type: StaticInt}
//! );
//! ```
//!
//! The actual value of the returned static ints will be lazily evaluated at [llir](debris_core::llir)
//!
//! A [context](debris_core::mir::MirContext) is used to keep track of all variables that have ben created, as well as their identifiers.

mod mir_nodes;
use std::fmt;

use itertools::Itertools;
pub use mir_nodes::{
    MirBranchIf, MirCall, MirGotoContext, MirJumpLocation, MirNode, MirReturnValue, MirUpdateValue,
    MirValue,
};

mod mir_visitor;
pub use mir_visitor::MirVisitor;

mod mir_context;
pub use mir_context::{
    ContextId, MirContext, MirContextInfo, NamespaceArena, NamespaceIndex, ReturnValues,
};

mod mir_context_kind;
pub use mir_context_kind::ContextKind;

mod mir_control_flow;
pub use mir_control_flow::ControlFlowMode;

mod mir_builder;
pub use mir_builder::{CachedFunctionSignature, MirBuilder};

mod mir_impl;
pub use mir_impl::Mir;
use rustc_hash::FxHashMap;

#[derive(Debug, Default)]
pub struct MirContextMap<'ctx> {
    contexts: FxHashMap<ContextId, MirContext<'ctx>>,
    pub main_context: Option<ContextId>,
    pub ticking_contexts: Vec<ContextId>,
}

impl<'ctx> MirContextMap<'ctx> {
    pub fn get_main_context(&self) -> &MirContext {
        self.get(
            self.main_context
                .expect("Expected the main context to be set"),
        )
    }

    #[inline]
    pub fn get(&self, id: ContextId) -> &MirContext {
        self.contexts
            .get(&id)
            .expect("Only valid ContextIds should exist")
    }

    #[inline]
    pub fn get_mut(&mut self, id: ContextId) -> &mut MirContext<'ctx> {
        self.contexts
            .get_mut(&id)
            .expect("Only valid ContextIds should exist")
    }
}

impl fmt::Display for MirContextMap<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_context(
            f: &mut fmt::Formatter<'_>,
            name: String,
            context: &MirContext,
        ) -> fmt::Result {
            f.write_fmt(format_args!("context {}:\n", name))?;
            for node in context.nodes.iter() {
                <MirNode as fmt::Display>::fmt(node, f)?;
                f.write_str("\n")?;
            }
            f.write_str("\n-----\n")
        }

        match self.main_context {
            Some(id) => fmt_context(f, "main".to_string(), self.get(id)),
            None => f.write_str("<No main context!>"),
        }?;

        for (id, context) in self
            .contexts
            .iter()
            .filter(|(id, _)| self.main_context.map_or(true, |main| main != **id))
            .sorted_by_key(|(id, _)| id.as_inner())
        {
            fmt_context(f, id.as_inner().to_string(), context)?;
        }
        Ok(())
    }
}
