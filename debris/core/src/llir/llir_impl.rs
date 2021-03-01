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
    /// The entry point of the program
    pub main_function: Function,
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

        let config = &main_context.compile_context.config;
        Ok(llir_functions.into_llir(config))
    }

    pub fn get_function_calls(&self) -> HashMap<BlockId, usize> {
        let mut stats = HashMap::default();
        for function in self
            .functions
            .iter()
            .chain(std::iter::once(&self.main_function))
        {
            for node in function.nodes() {
                node.iter(&mut |node| {
                    if let Node::Call(Call { id }) = node {
                        *stats.entry(*id).or_default() += 1;
                    }
                });
            }
        }
        stats.insert(self.main_function.id, 1);
        stats
    }
}

impl fmt::Display for Llir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let call_stats = self.get_function_calls();
        let fmt_function = &|func: &Function, f: &mut fmt::Formatter<'_>| {
            f.write_fmt(format_args!(
                "({} call(s)) - {}",
                call_stats[&func.id], func
            ))
        };

        f.write_str("Main ")?;
        fmt_function(&self.main_function, f)?;
        f.write_str("\n")?;

        for function in self.functions.iter().sorted_by_key(|func| func.id) {
            fmt_function(&function, f)?;
            f.write_str("\n")?;
        }

        Ok(())
    }
}

/// A function node as it is represented during the llir stage
#[derive(Debug)]
pub(crate) struct LLirFunction {
    pub(crate) returned_value: ObjectRef,
    pub(crate) nodes: PeepholeOptimizer,
}

/// Contains the already generated llir functions and
/// in the future potentialle other details
#[derive(Debug, Default)]
pub(crate) struct LlirFunctions {
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

        let optimizer = GlobalOptimizer::new(config, self.functions, main_function_id);
        let mut optimized_functions = optimizer.run();

        let main_function = optimized_functions
            .remove(&main_function_id)
            .expect("Could not find the main function");

        Llir {
            main_function,
            functions: optimized_functions
                .into_iter()
                .map(|(_, value)| value)
                .collect(),
            runtime: self.runtime,
        }
    }
}
