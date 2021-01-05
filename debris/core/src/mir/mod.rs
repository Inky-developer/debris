//! Mid-level intermediate representation
//!
//! Once the high lever representation has be created, it can be lowered into a mir.
//! The mir is quite simplistic and focuses on control flow.
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
pub use mir_nodes::{MirCall, MirGotoContext, MirNode, MirValue};

mod mir_visitor;
pub use mir_visitor::MirVisitor;

mod mir_context;
pub use mir_context::{
    ContextId, MirContext, MirContextInfo, MirInfo, MirNamespaceEntry, NamespaceArena,
};

mod mir_builder;
pub use mir_builder::{CachedFunctionSignature, MirBuilder};

mod mir_impl;
pub use mir_impl::Mir;
use rustc_hash::FxHashMap;

#[derive(Debug, Default)]
pub struct MirContextMap<'ctx> {
    contexts: FxHashMap<ContextId, MirContext<'ctx>>,
    main_context: Option<ContextId>,
}

impl<'ctx> MirContextMap<'ctx> {
    pub fn get_main_context(&self) -> &MirContext {
        self.get(
            self.main_context
                .expect("Expected the main context to be set"),
        )
    }

    pub fn get(&self, id: ContextId) -> &MirContext {
        self.contexts
            .get(&id)
            .expect("Only valid ContextIds should exist")
    }

    pub fn get_mut(&mut self, id: ContextId) -> &mut MirContext<'ctx> {
        self.contexts
            .get_mut(&id)
            .expect("Only valid ContextIds should exist")
    }
}
