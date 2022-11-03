use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    block_id::BlockId,
    llir_nodes::{Call, Node},
    opt::{
        global_opt::Commands,
        optimize_commands::{OptimizeCommand, OptimizeCommandKind},
        NodeId,
    },
};

/// This expensive optimization searches for common paths at conditionals.
///
/// A condition like `Condition {pos: Call(1), neg: Call(2)}`, where Block 2: `[..commands, Call(1)]`
/// Gets optimized such that the common calls to block 1 are removed in both calls and instead
/// inserted after the branch: `Condition {pos: Nop, neg: Call(2)}; Call(1)`, Block 2: `[..commands]`
/// Since both blocks which contain the common call are modified, it must be verified, that no other
/// context calls these.
///
/// This optimization is important, because without it, a lot of useless else statements would
/// be in the interpreter for a long time. This also reduces the code size. Additionally, it is easier
/// for humans to reason about linear code.
///
/// Todo: Reuse allocations
pub fn optimize_common_path(commands: &mut Commands) {
    for (node_id, node) in commands.optimizer.iter_nodes() {
        if let Node::Branch(branch) = node {
            let pos_branch = &*branch.pos_branch;
            let neg_branch = &*branch.neg_branch;
            if let (Node::Call(Call { id: pos_id }), Node::Call(Call { id: neg_id })) =
                (pos_branch, neg_branch)
            {
                let result = get_common_call(commands, *pos_id, *neg_id);
                if let Some((common_block, call_a, call_b)) = result {
                    if call_a != call_b {
                        commands
                            .commands
                            .push(OptimizeCommand::new(call_a, OptimizeCommandKind::Delete));

                        commands
                            .commands
                            .push(OptimizeCommand::new(call_b, OptimizeCommandKind::Delete));

                        commands.commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::InsertAfter(Node::Call(Call { id: common_block })),
                        ));

                        // This change must be committed before continuing with this optimization
                        return;
                    }
                }
            }
        }
    }
}

/// Extracts the first common function call in the
/// calls chains of `block_a` and `block_b`
fn get_common_call(
    commands: &Commands,
    block_a: BlockId,
    block_b: BlockId,
) -> Option<(BlockId, NodeId, NodeId)> {
    let a_calls = {
        let mut calls = FxHashMap::default();
        let mut current_block = block_a;
        loop {
            let function = commands.optimizer.get_function(current_block);
            if let Some(Node::Call(Call { id })) = function.nodes().last() {
                // Stop at recursion
                if calls.contains_key(id) {
                    break;
                }

                // Only insert if the node may be modified
                if commands.get_call_count(current_block) <= 1 {
                    calls.insert(*id, (current_block, function.nodes().len() - 1));
                }
                current_block = *id;
            } else {
                break;
            }
        }
        calls
    };

    let mut visited_functions = FxHashSet::default();

    let mut current_block = block_b;
    loop {
        let function = commands.optimizer.get_function(current_block);
        match function.nodes().last() {
            Some(Node::Call(Call { id })) => match a_calls.get(id) {
                Some(a_call) if commands.get_call_count(current_block) <= 1 => {
                    return Some((*id, *a_call, (current_block, function.nodes().len() - 1)));
                }
                _ => {
                    if visited_functions.contains(&current_block) {
                        break;
                    }
                    visited_functions.insert(*id);
                    current_block = *id;
                }
            },
            _ => break,
        }
    }

    None
}
