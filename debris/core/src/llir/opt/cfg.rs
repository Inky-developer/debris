//! Control flow graph implementation (ToDo)

use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasher,
};

use crate::llir::{
    llir_nodes::{Call, Function, Node},
    utils::BlockId,
};

/// Detects whether a function is inside of a loop
#[derive(Debug, Default)]
pub struct LoopDetector {
    visited_functions: HashSet<BlockId>,
    pending_functions: HashSet<BlockId>,
}

impl LoopDetector {
    /// Returns true if `block_id` is part of an unconditionally infinite loop
    pub fn is_infinite_loop<H>(
        &mut self,
        functions: &HashMap<BlockId, Function, H>,
        block_id: BlockId,
    ) -> bool
    where
        H: BuildHasher,
    {
        self.visited_functions.clear();
        self.pending_functions.clear();
        self.pending_functions.insert(block_id);

        while let Some(current_function_id) = self.pending_functions.iter().next() {
            let current_function_id = *current_function_id;
            self.pending_functions.remove(&current_function_id);
            self.visited_functions.insert(current_function_id);

            let function = &functions[&current_function_id];
            let mut found_recursive_call = false;
            for node in function.nodes() {
                if let Node::Call(Call { id }) = node {
                    if !self.visited_functions.contains(id) {
                        self.pending_functions.insert(*id);
                    }
                    if id == &block_id {
                        found_recursive_call = true;
                    }
                }

                if found_recursive_call {
                    return true;
                }
            }
        }
        false
    }
}
