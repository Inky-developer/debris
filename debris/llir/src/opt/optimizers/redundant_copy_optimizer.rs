//! TODO: implement this properly

use crate::{
    item_id::ItemId,
    llir_nodes::{BinaryOperation, FastStore, Node, VariableAccess},
    minecraft_utils::ScoreboardValue,
    opt::{
        global_opt::{Commands, Optimizer},
        optimize_commands::{OptimizeCommand, OptimizeCommandDeque, OptimizeCommandKind},
    },
};

/// A node which copies a value `a` to `b` is often redundant.
/// This is especially important, because the compiler tends to generate operations
/// in the shape of `let temp = a; operation_with_temp(temp); a = temp`.
/// This optimizer aggressively removes copy instructions (`let temp = a`) and changes all
/// subsequent reads to the previous variable (`a`).
#[derive(Default)]
pub struct RedundantCopyOptimizer {
    pending_commands: OptimizeCommandDeque<OptimizeCommand>,
}

impl Optimizer for RedundantCopyOptimizer {
    fn optimize(&mut self, commands: &mut Commands) {
        for (function_id, function) in &commands.optimizer.functions {
            'node_loop: for (idx, node) in function.nodes().iter().enumerate() {
                let (temp_id, original_id, original_scoreboard, is_bin_op) = match node {
                    Node::FastStore(FastStore {
                        scoreboard: _,
                        id: temp_id,
                        value: ScoreboardValue::Scoreboard(original_scoreboard, original_id),
                    }) => (temp_id, original_id, original_scoreboard, false),
                    Node::BinaryOperation(BinaryOperation {
                        scoreboard: _,
                        id: temp_id,
                        lhs: ScoreboardValue::Scoreboard(original_scoreboard, original_id),
                        rhs: _,
                        operation: _,
                    }) => (temp_id, original_id, original_scoreboard, true),
                    _ => continue,
                };

                if temp_id == original_id {
                    continue;
                }

                self.pending_commands.clear();
                let mut optimization_success = false;
                let total_temp_reads = commands.get_reads(*temp_id);
                let mut encountered_temp_reads = 0;
                let mut encountered_original_reads = 0;
                let mut optimization_modifies_original_value = false;
                // If the first node is a binary operation, the original value gets modified.
                let mut modifies_original_value = is_bin_op;
                let mut reads_from_original_value = false;
                for (node_id, node) in commands.optimizer.iter_at(&(*function_id, idx)) {
                    let mut reads_from_original = false;
                    let mut write_to_original = false;
                    let mut reads_from_copy = false;
                    let mut write_to_copy = false;

                    // If this node is a function, try to not abort this optimization
                    // TODO: Maybe do a dfs check for lower abortion rate
                    if let Node::Call(call) = node {
                        let called = call.id;
                        let func_writes_to_original = commands
                            .stats
                            .function_parameters
                            .get(called, *original_id)
                            .is_write();
                        // The number of functions this function calls
                        let called_function_calls = commands
                            .stats
                            .call_graph
                            .get_called_functions(called)
                            .count();
                        if called_function_calls > 0 || func_writes_to_original {
                            break 'node_loop;
                        }
                    }

                    node.variable_accesses(&mut |access| match access {
                        VariableAccess::Read(ScoreboardValue::Scoreboard(_, id)) => {
                            if id == original_id {
                                encountered_original_reads += 1;
                                reads_from_original = true;
                            } else if id == temp_id {
                                encountered_temp_reads += 1;
                                reads_from_copy = true;
                            }
                        }
                        VariableAccess::Write(id, _) => {
                            if id == original_id {
                                write_to_original = true;
                            } else if id == temp_id {
                                write_to_copy = true;
                            }
                        }
                        VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, id)) => {
                            if id == original_id {
                                encountered_original_reads += 1;
                                write_to_original = true;
                                reads_from_original = true;
                            } else if id == temp_id {
                                encountered_temp_reads += 1;
                                write_to_copy = true;
                                reads_from_copy = true;
                            }
                        }
                        _ => {}
                    });

                    if write_to_copy {
                        self.pending_commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::ChangeWrite(*original_id),
                        ));
                        optimization_modifies_original_value = true;
                    }
                    if reads_from_copy {
                        self.pending_commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::ChangeReads(
                                *temp_id,
                                ScoreboardValue::Scoreboard(*original_scoreboard, *original_id),
                            ),
                        ));
                    }
                    if reads_from_original {
                        reads_from_original_value = true;
                    }
                    let is_copy_back = match node {
                        Node::FastStore(FastStore {
                            scoreboard: _,
                            id: dest,
                            value: ScoreboardValue::Scoreboard(_, src),
                        }) => dest == original_id && src == temp_id,
                        _ => false,
                    };
                    // If the original value gets modified, stop the search
                    if write_to_original {
                        if is_copy_back {
                            modifies_original_value = true;
                            if encountered_temp_reads == total_temp_reads {
                                break;
                            }
                        } else {
                            // error case
                            continue 'node_loop;
                        }
                    }
                }

                // If the original value is accessed, but this optimization modifies the original value,
                // the entire optimization becomes invalid.
                if reads_from_original_value && modifies_original_value {
                    continue;
                }

                // If no function has a dependency on the original variable,
                // the optimizer is free to inline it.
                let is_unused_after = |commands: &Commands, id: ItemId| {
                    commands.get_reads(id) == 0
                        || ((!commands.stats.function_parameters.is_dependency(id))
                            && (commands
                                .optimizer
                                .iter_at(&(*function_id, idx))
                                .all(|(_, node)| !node.reads_from(id))))
                };

                // If the value just gets copied to be read from, in the same function,
                // this optimization is valid
                if !optimization_success
                    && !modifies_original_value
                    && !optimization_modifies_original_value
                    && !commands.stats.function_parameters.is_dependency(*temp_id)
                {
                    optimization_success = true;
                }

                // println!(
                //     "!!!!!{:?}, {}, {}, {:?}",
                //     original_id,
                //     optimization_success,
                //     is_unused_after(commands, *original_id),
                //     commands.stats.function_parameters
                // );

                // what a lovely condition
                if optimization_success
                    || (is_unused_after(commands, *original_id)
                        && ((encountered_temp_reads == total_temp_reads
                            && (!matches!(node, Node::BinaryOperation(_))
                                || optimization_modifies_original_value))
                            || (is_unused_after(commands, *original_id)
                                && !commands.stats.function_parameters.is_dependency(*temp_id))))
                {
                    // If this code runs, the optimization was successful
                    // Now write all the nodes.
                    // First, update the copy node:
                    match node {
                        Node::BinaryOperation(BinaryOperation {
                            scoreboard,
                            id: _,
                            lhs,
                            rhs,
                            operation,
                        }) => {
                            commands.commands.push(OptimizeCommand::new(
                                (*function_id, idx),
                                OptimizeCommandKind::Replace(Node::BinaryOperation(
                                    BinaryOperation {
                                        id: *original_id,
                                        scoreboard: *scoreboard,
                                        lhs: *lhs,
                                        operation: *operation,
                                        rhs: *rhs,
                                    },
                                )),
                            ));
                        }
                        Node::FastStore(_) => commands.commands.push(OptimizeCommand::new(
                            (*function_id, idx),
                            OptimizeCommandKind::Delete,
                        )),
                        _ => unreachable!(),
                    }

                    // Then, modify all the changed node
                    commands.commands.append(&mut self.pending_commands);
                    // Continue at the next function
                    break;
                }
            }
        }
    }
}
