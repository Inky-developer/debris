use crate::llir::{
    llir_nodes::{Branch, FastStore, FastStoreFromResult, Node, VariableAccess},
    opt::{
        global_opt::{Commands, Optimizer},
        optimize_commands::{OptimizeCommand, OptimizeCommandKind},
        variable_metadata::{Hint, ValueHints},
    },
    utils::ScoreboardValue,
};

/// Optimizes nodes which are const-evaluatable.
/// This optimizer tracks all const assignments to variables in
/// a given function and replaces reads from const variables by their
/// const value. Also contains functionality to evaluate [BinaryOperation](crate::llir::llir_nodes::BinaryOperation)
/// and [Condition](crate::llir::llir_nodes::Condition).
/// In order to be more efficient, this optimizer optimizes an entire function.
/// This means that the current state must always be synced correctly!
#[derive(Default)]
pub struct ConstOptimizer {
    value_hints: ValueHints,
}

impl Optimizer for ConstOptimizer {
    fn optimize(&mut self, commands: &mut Commands) {
        for function_iter in commands.optimizer.iter_functions() {
            self.value_hints.clear_all();
            for (node_id, node) in function_iter {
                let mut did_optimize = false;
                let commands_vec = &mut *commands.commands;
                let variable_information = &commands.stats.variable_information;
                node.variable_accesses(&mut |access| {
                    if let VariableAccess::Read(ScoreboardValue::Scoreboard(_, id)) = access {
                        let hint = self.value_hints.get_hint(id).exact();
                        let global_const = variable_information[id].constant_value;
                        if let Some(exact_value) = hint.or(global_const) {
                            commands_vec.push(OptimizeCommand::new(
                                node_id,
                                OptimizeCommandKind::ChangeReads(
                                    *id,
                                    ScoreboardValue::Static(exact_value),
                                ),
                            ));
                            self.value_hints.update_hints(node);
                            did_optimize = true;
                        }
                    }
                });

                // Keep in mind that the new node must be update in self.value_hints!
                if !did_optimize {
                    // Evaluate static binary operations
                    match node {
                        Node::BinaryOperation(bin_op) => {
                            match self.value_hints.static_binary_operation(bin_op) {
                                Some(result) => {
                                    commands.commands.push(OptimizeCommand::new(
                                        node_id,
                                        OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                                            id: bin_op.id,
                                            scoreboard: bin_op.scoreboard,
                                            value: ScoreboardValue::Static(result),
                                        })),
                                    ));
                                    self.value_hints.set_hint(bin_op.id, Hint::Exact(result))
                                }
                                None => {
                                    self.value_hints.update_hints(node);
                                }
                            }
                        }
                        Node::FastStoreFromResult(FastStoreFromResult {
                            scoreboard,
                            id,
                            command,
                        }) => {
                            if let Node::Condition(condition) = &**command {
                                if let Some(result) = self.value_hints.static_condition(condition) {
                                    let value = if result { 1 } else { 0 };
                                    commands.commands.push(OptimizeCommand::new(
                                        node_id,
                                        OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                                            id: *id,
                                            scoreboard: *scoreboard,
                                            value: ScoreboardValue::Static(value),
                                        })),
                                    ));
                                    self.value_hints.set_hint(*id, Hint::Exact(value));
                                } else {
                                    self.value_hints.update_hints(node);
                                }
                            } else {
                                self.value_hints.update_hints(node);
                            }
                        }
                        Node::Branch(Branch {
                            condition,
                            pos_branch,
                            neg_branch,
                        }) => {
                            if let Some(result) = self.value_hints.static_condition(condition) {
                                commands.commands.push(OptimizeCommand::new(
                                    node_id,
                                    OptimizeCommandKind::InlineBranch(result),
                                ));
                                let branch = if result { pos_branch } else { neg_branch };
                                self.value_hints.update_hints(&branch);
                            } else {
                                self.value_hints.update_hints(node);
                            }
                        }
                        _ => {
                            self.value_hints.update_hints(node);
                        }
                    }
                    // self.value_hints.update_hints(node);
                }
            }
        }
    }
}
