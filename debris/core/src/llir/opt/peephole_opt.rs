use crate::llir::{
    llir_nodes::{BinaryOperation, Branch, Call, Condition, FastStore, FastStoreFromResult, Node},
    utils::{ScoreboardComparison, ScoreboardValue},
};

use super::variable_metadata::{Hint, ValueHints};

/// A just-in-time peephole optimizer.
///
/// This means, that the optimizer optimizes nodes as they are beeing addded,
/// while also having access to the previously emitted nodes.
///
/// A single `PeepholeOptimizer` can only be active in a single context
/// (aka a single .mcfunction file)
#[derive(Debug, Default)]
pub(crate) struct PeepholeOptimizer {
    nodes: Vec<Node>,

    /// Information about the possible values of runtime variables
    value_hints: ValueHints,
}

impl PeepholeOptimizer {
    /// Adds this node to the collection and optimizes it on the fly
    pub fn push(&mut self, node: Node) {
        self.optimize_and_insert(node);
        // self.nodes.push(node);
    }

    /// Drops this instance and returns the wrapped nodes
    pub fn take(self) -> Vec<Node> {
        self.nodes
    }
}

impl PeepholeOptimizer {
    /// Optimizes a node and the previous nodes and pushes the new node to the collection of nodes
    ///
    /// This operation may affect already pushed nodes.
    fn optimize_and_insert(&mut self, node: Node) {
        let node = self.optimize(node);

        self.nodes.push(node);
    }

    fn optimize(&mut self, node: Node) -> Node {
        self.update_hints(&node);
        match node {
            Node::FastStore(FastStore {
                id,
                scoreboard,
                value: ScoreboardValue::Scoreboard(_, other_id),
                ..
            }) => {
                if let Hint::Exact(value) = self.value_hints.get_hint(&other_id) {
                    Node::FastStore(FastStore {
                        id,
                        scoreboard,
                        value: ScoreboardValue::Static(value),
                    })
                } else {
                    node
                }
            }
            Node::Branch(branch) => self.optimize_branch(branch),
            other => other,
        }
    }

    /// Updates the hints for all variables that this node modifies
    fn update_hints(&mut self, node: &Node) {
        match &node {
            Node::Function(_function) => {}
            Node::FastStore(FastStore { id, value, .. }) => match value {
                ScoreboardValue::Static(static_value) => {
                    self.value_hints.set_hint(*id, Hint::Exact(*static_value));
                }
                ScoreboardValue::Scoreboard(_scoreboard, other_id) => {
                    let other_value_hint = self.value_hints.get_hint(other_id);
                    self.value_hints.set_hint(*id, other_value_hint);
                }
            },
            Node::FastStoreFromResult(FastStoreFromResult { id, .. }) => {
                self.value_hints.clear_hint(*id);
            }
            // ToDo: Once the `BinaryOperation::Copy` gets used, special case that
            Node::BinaryOperation(BinaryOperation { id, .. }) => {
                self.value_hints.clear_hint(*id);
            }
            // Unfortunately calls cannot be analyzed right now
            // ToDo: Make calls not clear all hints
            Node::Call(Call { .. }) => self.value_hints.clear_all(),
            Node::Condition(_) => {}
            Node::Branch(_) => {}
            // Any execute node that modifies a value that belongs to debris cause
            // undefined behavior!
            Node::Execute(_) => {}
            Node::Write(_) => {}
        }
    }

    fn optimize_branch(&mut self, branch: Branch) -> Node {
        // Match this specific condition which is often emitted for if-statements
        if let Branch {
            condition:
                Condition::Compare {
                    comparison: ScoreboardComparison::Equal,
                    lhs: ScoreboardValue::Scoreboard(_, id),
                    rhs: ScoreboardValue::Static(1),
                },
            ..
        } = &branch
        {
            if let Some(last_statement) = self.nodes.last() {
                if let Node::FastStoreFromResult(FastStoreFromResult {
                    id: other_id,
                    command,
                    ..
                }) = last_statement
                {
                    // Match only if the last statement set the condition of the branch
                    if id == other_id {
                        // At last, check if the last statement was an assignment with of a condition
                        if let Node::Condition(condition) = command.as_ref() {
                            if condition.is_simple() {
                                // oof, is that really necessary?
                                let condition = {
                                    match self.nodes.pop().unwrap() {
                                        Node::FastStoreFromResult(FastStoreFromResult {
                                            command,
                                            ..
                                        }) => match *command {
                                            Node::Condition(condition) => condition,
                                            _other => unreachable!("Must be a condition"),
                                        },
                                        _other => unreachable!("Must be a FastStoreFromResult"),
                                    }
                                };

                                let Branch {
                                    pos_branch,
                                    neg_branch,
                                    ..
                                } = branch;

                                return Node::Branch(Branch {
                                    condition,
                                    pos_branch,
                                    neg_branch,
                                });
                            }
                        }
                    }
                }
            }
        }

        Node::Branch(branch)
    }
}
