use crate::llir::{
    llir_nodes::{Branch, Condition, FastStoreFromResult, Node, VariableAccessMut},
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
pub struct PeepholeOptimizer {
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

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
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
        self.value_hints.update_hints(&node);

        match node {
            Node::Branch(branch) => self.optimize_branch(branch),
            mut other => {
                other.variable_accesses_mut(&mut |access| {
                    if let VariableAccessMut::Read(value) = access {
                        if let ScoreboardValue::Scoreboard(_, id) = value {
                            if let Hint::Exact(exact_value) = self.value_hints.get_hint(id) {
                                *value = ScoreboardValue::Static(exact_value);
                            }
                        }
                    }
                });
                other
            }
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
            if let Some(Node::FastStoreFromResult(FastStoreFromResult {
                id: other_id,
                command,
                ..
            })) = self.nodes.last()
            {
                // Match only if the last statement has set the condition of the branch
                if id == other_id {
                    // At last, check if the last statement was an assignment with of a condition
                    if let Node::Condition(condition) = &**command {
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

        Node::Branch(branch)
    }
}
