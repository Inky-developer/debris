//! Contains two optimizer that can operate on the llir:
//! The [peephole optimizer](peephole_opt::PeepholeOptimizer) which optimizes
//! every emitted node on its own (but is also able to look at previous nodes)
//! and the [global optimizer](global_opt::GlobalOptimizer)

use super::block_id::BlockId;

pub mod call_graph;
pub mod code_stats;
mod function_parameters;
pub mod global_opt;
mod logger;
pub mod optimize_commands;
pub mod optimizers;
pub mod peephole_opt;
pub mod variable_metadata;

/// The optimizer can uniquely identify each node with this type
pub type NodeId = (BlockId, usize);
