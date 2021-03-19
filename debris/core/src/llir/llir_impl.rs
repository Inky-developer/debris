use std::{collections::HashMap, fmt};

use itertools::Itertools;
use rustc_hash::FxHashMap;

use super::{
    llir_nodes::{Call, Function, Node},
    opt::{global_opt::GlobalOptimizer, peephole_opt::PeepholeOptimizer},
    utils::BlockId,
    LlirBuilder, Runtime,
};
use crate::{
    error::Result,
    mir::{ContextId, MirContextMap, NamespaceArena},
    Config, ObjectRef,
};

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug)]
pub struct Llir {
    /// The functions which were created, excluding the main function
    pub functions: Vec<Function>,
    /// The runtime, which stores resources
    pub runtime: Runtime,
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn from_mir(contexts: &MirContextMap, namespaces: &mut NamespaceArena) -> Result<Llir> {
        let mut llir_functions = LlirFunctions::default();
        let main_context = contexts.get_main_context();

        let builder = LlirBuilder::new(main_context, namespaces, contexts, &mut llir_functions);
        let main_function = builder.current_function;
        builder.build()?;

        llir_functions.main_function = Some(main_function);
        llir_functions.runtime.add_on_load(main_function);

        let config = &main_context.compile_context.config;
        Ok(llir_functions.into_llir(config))
    }

    pub fn get_function_calls(&self) -> HashMap<BlockId, usize> {
        let mut stats = HashMap::default();
        for function in &self.functions {
            for node in function.nodes() {
                node.iter(&mut |node| {
                    if let Node::Call(Call { id }) = node {
                        *stats.entry(*id).or_default() += 1;
                    }
                });
            }
        }

        for on_load_block in &self.runtime.load_blocks {
            *stats.entry(*on_load_block).or_default() += 1;
        }

        for on_tick_block in &self.runtime.scheduled_blocks {
            *stats.entry(*on_tick_block).or_default() += 1;
        }

        stats
    }
}

impl fmt::Display for Llir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let call_stats = self.get_function_calls();
        let fmt_function = &|func: &Function, f: &mut fmt::Formatter<'_>| {
            f.write_fmt(format_args!(
                "({} call(s)) - {}",
                call_stats.get(&func.id).unwrap_or(&0),
                func
            ))
        };

        for function in self.functions.iter().sorted_by_key(|func| func.id) {
            fmt_function(&function, f)?;
            f.write_str("\n")?;
        }

        Ok(())
    }
}

/// A function node as it is represented during the llir stage
#[derive(Debug)]
pub struct LLirFunction {
    pub returned_value: ObjectRef,
    pub nodes: PeepholeOptimizer,
}

/// Contains the already generated llir functions and
/// in the future potentialle other details
#[derive(Debug, Default)]
pub struct LlirFunctions {
    pub runtime: Runtime,
    pub functions: FxHashMap<BlockId, LLirFunction>,
    /// Mapping from context to function
    context_to_function: FxHashMap<(ContextId, usize), BlockId>,
    pub main_function: Option<BlockId>,
    function_id_counter: usize,
}

impl LlirFunctions {
    pub fn add(&mut self, block: BlockId, function: LLirFunction) {
        let prev_block = self.functions.insert(block, function);
        assert!(prev_block.is_none(), "Duplicate block with id {:?}", block);
    }

    pub fn get_function(&mut self, id: &BlockId) -> Option<&mut LLirFunction> {
        self.functions.get_mut(id)
    }

    pub fn is_context_registered(&self, context: &(ContextId, usize)) -> bool {
        self.context_to_function.get(context).is_some()
    }

    pub fn block_for(&mut self, context: (ContextId, usize)) -> BlockId {
        match self.context_to_function.get(&context) {
            Some(block_id) => *block_id,
            None => {
                let value = BlockId(self.function_id_counter);
                self.function_id_counter += 1;
                self.context_to_function.insert(context, value);
                value
            }
        }
    }

    fn into_llir(self, config: &Config) -> Llir {
        let main_function_id = self.main_function.expect("No main function");
        let optimizer =
            GlobalOptimizer::new(config, &self.runtime, self.functions, main_function_id);
        let functions = optimizer
            .run()
            .into_iter()
            .map(|(_, function)| function)
            .collect();

        Llir {
            functions,
            runtime: self.runtime,
        }
    }
}
