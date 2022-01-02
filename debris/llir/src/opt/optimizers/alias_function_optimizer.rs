use crate::{
    llir_nodes::{Call, Node},
    opt::{
        global_opt::Commands,
        optimize_commands::{OptimizeCommand, OptimizeCommandKind},
    },
};

/// This optimizer handles functions that alias other function.
/// An aliasing function consists of only one node, which calls another function.
/// If such a function is found, it gets deleted and all references to it get moved
/// to the aliased function.
/// This prevents the optimizer from inlining into an aliasing function, reducing the code size.
pub fn optimize_alias_function(commands: &mut Commands) {
    for (function_id, function) in &commands.optimizer.functions {
        if let [Node::Call(Call { id })] = function.nodes() {
            if id == function_id
                || commands.get_call_count(*id) <= 1
                || commands.optimizer.runtime.contains(function_id)
            {
                continue;
            }

            // found an aliasing function, now remove all references to it
            commands.commands.push(OptimizeCommand::new(
                (*function_id, 0),
                OptimizeCommandKind::RemoveAliasFunction(*id),
            ));
            return;
        }
    }
}
