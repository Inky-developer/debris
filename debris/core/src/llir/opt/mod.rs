//! Contains two optimizer that can operate on the llir:
//! The [peephole optimizer](peephole_opt::PeepholeOptimizer) which optimizes
//! every emmited node on its own (but is also able to look at previous nodes)
//! and the [global optimizer](global_opt::GlobalOptimizer)

use super::utils::BlockId;

pub(crate) mod call_graph;
mod code_stats;
mod function_parameters;
pub(crate) mod global_opt;
pub(crate) mod optimize_commands;
pub(crate) mod optimizers;
pub(crate) mod peephole_opt;
pub(crate) mod variable_metadata;

/// The optimizer can uniquely identify each node with this type
pub(crate) type NodeId = (BlockId, usize);
