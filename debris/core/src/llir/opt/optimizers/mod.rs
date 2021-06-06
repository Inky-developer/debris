//! This module contains various types implementing the [Optimizer](super::global_opt::Optimizer) trait.

mod alias_function_optimizer;
mod arithmetic_optimizer;
mod callchain_optimizer;
mod common_path_optimizer;
mod const_optimizer;
mod redundancy_optimizer;

pub use alias_function_optimizer::optimize_alias_function;
pub use arithmetic_optimizer::simple_arithmetic_optimization;
pub use callchain_optimizer::optimize_call_chain;
pub use common_path_optimizer::optimize_common_path;
pub use const_optimizer::ConstOptimizer;
pub use redundancy_optimizer::RedundancyOptimizer;
