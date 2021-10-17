use rustc_hash::FxHashSet;

use crate::{
    llir_nodes::{Call, Node},
    opt::{
        global_opt::Commands,
        optimize_commands::{OptimizeCommand, OptimizeCommandKind},
    },
};

/// Inlines the function call chain.
/// This optimizer only runs once, at the start.
pub fn optimize_call_chain(commands: &mut Commands) {
    // Cannot inline a function which has a call that was already inline without updating stats
    let mut encountered_functions = FxHashSet::default();
    encountered_functions.reserve(commands.optimizer.functions.len());
    for id in commands
        .stats
        .call_graph
        .iter_dfs(commands.optimizer.runtime.root_blocks())
    {
        // If this function only directs to another function, inline this function
        let function = commands.optimizer.get_function(&id);
        if let Some(Node::Call(Call { id: other_function })) = function.nodes().last() {
            if other_function != &id && !encountered_functions.contains(&id) {
                let idx = function.nodes.len() - 1;
                commands.commands.push(OptimizeCommand::new(
                    (id, idx),
                    OptimizeCommandKind::InlineFunction,
                ));
                encountered_functions.insert(*other_function);
            }
        }
    }
}
