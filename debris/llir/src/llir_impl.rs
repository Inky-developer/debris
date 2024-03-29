use std::{collections::HashMap, fmt};

use debris_common::{CompileContext, Ident};
use debris_error::Result;
use debris_mir::Mir;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{llir_builder::LlirBuilder, CodeStats, ObjectRef};

use super::{block_id::BlockId, llir_nodes::Function, type_context::TypeContext, Runtime};

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug)]
pub struct Llir {
    /// The functions which were created, excluding the main function
    pub functions: FxHashMap<BlockId, Function>,
    /// The entry point
    pub entry_function: BlockId,
    /// The runtime, which stores resources
    pub runtime: Runtime,
    /// Some statistics interesting for the backend
    pub stats: CodeStats,
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn new(
        ctx: &CompileContext,
        extern_items_factory: impl Fn(&TypeContext) -> HashMap<Ident, ObjectRef>,
        mir: &Mir,
    ) -> Result<Llir> {
        let builder = LlirBuilder::new(
            ctx,
            extern_items_factory,
            &mir.extern_items,
            &mir.namespace,
            &mir.return_values_arena,
        )?;
        builder.build(mir.entry_context, &mir.contexts)
    }
}

impl fmt::Display for Llir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let call_stats = &self.stats.function_calls;
        let fmt_function = &|func: &Function, f: &mut fmt::Formatter<'_>| {
            let num_calls = call_stats.get(&func.id).unwrap_or(&0);
            write!(f, "({num_calls} call(s)) - {func}")
        };

        for function in self.functions.values().sorted_by_key(|func| func.id) {
            fmt_function(function, f)?;
            f.write_str("\n")?;
        }

        Ok(())
    }
}
