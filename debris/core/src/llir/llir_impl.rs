use rustc_hash::FxHashMap;

use super::{
    llir_nodes::Function,
    opt::{global_opt::GlobalOptimizer, peephole_opt::PeepholeOptimizer},
    utils::BlockId,
    LlirBuilder,
};
use crate::{
    error::Result,
    mir::{ContextId, MirContextMap, NamespaceArena},
    ObjectRef,
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
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn from_mir(contexts: &MirContextMap, namespaces: &mut NamespaceArena) -> Result<Llir> {
        let mut llir_functions = LlirFunctions::default();
        let main_context = contexts.get_main_context();

        LlirBuilder::new(main_context, namespaces, contexts, &mut llir_functions).build()?;

        Ok(llir_functions.into())
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
    pub functions: FxHashMap<BlockId, LLirFunction>,
    /// Mapping from context to function
    pub context_to_function: FxHashMap<ContextId, BlockId>,
    pub main_function: Option<BlockId>,
    function_id_counter: usize,
}

impl LlirFunctions {
    pub fn add(&mut self, id: BlockId, function: LLirFunction) {
        // The last function must be the main function
        self.main_function = Some(id);

        let prev_function = self.functions.insert(id, function);
        assert!(
            prev_function.is_none(),
            "Duplicate function with id {:?}",
            id
        );
    }

    pub fn get_function(&mut self, id: &BlockId) -> &mut LLirFunction {
        self.functions.get_mut(id).expect("Could not find function")
    }

    pub fn next_id(&mut self) -> BlockId {
        let value = self.function_id_counter;
        self.function_id_counter += 1;
        BlockId(value)
    }
}

impl Into<Llir> for LlirFunctions {
    fn into(self) -> Llir {
        let main_function_id = self.main_function.expect("No main function");
        let optimizer = GlobalOptimizer::new(self.functions, main_function_id);
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
        }
    }
}
