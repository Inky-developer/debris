use std::collections::hash_map::Entry;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    llir_nodes::{Call, Function, Node, VariableAccess},
    utils::{BlockId, ItemId, ScoreboardValue},
    CallGraph, Runtime,
};

use super::{function_parameters::FunctionParameters, variable_metadata::VariableUsage, NodeId};

/// tracks statistics about the global code which can be used
/// to allow some optimizations
#[derive(Debug)]
pub struct CodeStats {
    pub variable_information: FxHashMap<ItemId, VariableUsage>,
    pub function_calls: FxHashMap<BlockId, usize>,
    pub call_graph: CallGraph,
    /// Tracks which parameters a function takes.
    /// Due to the incremental updates, a function can declare to read a variable,
    /// Even if that is not the case anymore. It will never declare to not read a variable
    /// if it does so, though.
    pub function_parameters: FunctionParameters,
    /// This local variable is cached, so no repeated allocations are required
    visited_functions: FxHashSet<BlockId>,
}

impl CodeStats {
    pub fn new(call_graph: CallGraph) -> Self {
        CodeStats {
            variable_information: Default::default(),
            function_calls: Default::default(),
            call_graph,
            function_parameters: Default::default(),
            visited_functions: Default::default(),
        }
    }

    pub fn clear(&mut self) {
        self.variable_information.clear();
        self.function_calls.clear();
        self.function_parameters.clear();
        self.visited_functions.clear();
    }

    /// Updates the variable reads and writes.
    /// The iterator does not technically need to give mutable nodes.
    /// However, due to some rust limitations (how to abstract over & and &mut at the same time?)
    /// Mutable references are required.
    pub fn update(&mut self, runtime: &Runtime, functions: &FxHashMap<BlockId, Function>) {
        self.clear();

        self.visited_functions.reserve(functions.len());
        let mut pending_functions = FxHashSet::default();

        for on_load_block in &runtime.load_blocks {
            *self.function_calls.entry(*on_load_block).or_default() += 1;
            pending_functions.insert(*on_load_block);
        }

        for on_tick_block in &runtime.scheduled_blocks {
            *self.function_calls.entry(*on_tick_block).or_default() += 1;
            pending_functions.insert(*on_tick_block);
        }

        while let Some(function_id) = pending_functions.iter().next().copied() {
            pending_functions.remove(&function_id);
            self.visited_functions.insert(function_id);

            let function = functions.get(&function_id).unwrap();
            for node in function.nodes() {
                self.update_node(
                    node,
                    VariableUsage::add_read,
                    VariableUsage::add_write,
                    None,
                );
                // Also update calling statistics
                node.iter(&mut |inner_node| {
                    if let Node::Call(Call { id }) = inner_node {
                        *self.function_calls.entry(*id).or_default() += 1;
                        if !self.visited_functions.contains(id) {
                            pending_functions.insert(*id);
                        }
                    }
                });
                // It is important that writes are not written if there exist reads,
                // since reads are more important to store heres
                node.variable_accesses(&mut |access| match access {
                    VariableAccess::Write(id, _) => {
                        if !node.reads_from(*id) {
                            self.function_parameters.set_write_weak(function_id, *id);
                        }
                    }
                    VariableAccess::Read(ScoreboardValue::Scoreboard(_, id))
                    | VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, id)) => {
                        self.function_parameters.set_read_weak(function_id, *id);
                    }
                    _ => {}
                });
            }
        }
    }

    pub fn add_node(&mut self, node: &Node, id: &NodeId) {
        self.update_node(
            node,
            VariableUsage::add_read,
            VariableUsage::add_write,
            Some(id.0),
        );

        node.iter(&mut |inner_node| {
            if let Node::Call(Call { id: call_id }) = inner_node {
                *self.function_calls.entry(*call_id).or_default() += 1;
                self.call_graph.modify_call(id.0, *call_id, 1);
            }
        });
    }

    pub fn remove_node(&mut self, node: &Node, id: &NodeId) {
        self.update_node(
            node,
            VariableUsage::remove_read,
            |usage, _| usage.remove_write(),
            None,
        );

        node.iter(&mut |inner_node| {
            if let Node::Call(Call { id: call_id }) = inner_node {
                match self.function_calls.entry(*call_id) {
                    Entry::Occupied(entry) => *entry.into_mut() -= 1,
                    Entry::Vacant(_) => unreachable!("This function should exist"),
                }
                self.call_graph.modify_call(id.0, *call_id, -1);
                // println!("Removing call to {} - now at {} calls", self.function_calls.en)
            }
        });
    }

    fn update_node<FR, FW>(
        &mut self,
        node: &Node,
        read: FR,
        write: FW,
        parameter_read: Option<BlockId>,
    ) where
        FR: Fn(&mut VariableUsage),
        FW: Fn(&mut VariableUsage, Option<i32>),
    {
        node.variable_accesses(&mut |access| match access {
            VariableAccess::Read(ScoreboardValue::Scoreboard(_, value)) => {
                if let Some(function) = parameter_read {
                    self.function_parameters.set_read(function, *value);
                }
                read(self.variable_information.entry(*value).or_default());
            }
            VariableAccess::Write(value, const_val) => write(
                self.variable_information.entry(*value).or_default(),
                const_val,
            ),
            VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, value)) => {
                if let Some(function) = parameter_read {
                    self.function_parameters.set_read(function, *value);
                }
                read(self.variable_information.entry(*value).or_default());
                write(self.variable_information.entry(*value).or_default(), None);
            }
            _ => {}
        });
    }
}
